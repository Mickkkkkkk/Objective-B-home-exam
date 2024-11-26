module Interpreter where

import Syntax
import Control.Monad
import Control.Arrow (second)

type Output      = [String]
type ErrorMessage = String
data RuntimeError =
    UnboundVariable VariableName
  | BadProcedure    ProcedureName
  | BadArgument     ErrorMessage
  | BadOperands     Value Value
  | TypeMismatch    Value DeclType
  | RedeclarationError   VariableName  
  | DivisionByZero
  | BadOperation Value OperationSymbol Value
  | InterpreterError ErrorMessage -- An error which is thrown for a legal program due to 
                                  -- the interpreter being faulty. Ideally should newer
                                  -- be produced.
  deriving (Eq, Show)

-------------------------- ENVIRONMENTS --------------------------

type ProcedureEnvironment = [(ProcedureName, (DeclType, ProcedureParameters, ProcedureBody))]
type VariableEnvironment = [Frame] 
type Frame = [(VariableName, (DeclType, Value))]

-- Finds the "nearest" variable named "name". If not found, 
-- give an error.
mutateVarEnv :: VariableName -> Value -> VariableEnvironment -> 
                Either RuntimeError VariableEnvironment
mutateVarEnv name _ [] = Left $ UnboundVariable name
mutateVarEnv name val (frame : rest) =
  case mutateFrame name val frame of
    Right newFrame -> Right $ newFrame : rest
    Left _ -> 
      case mutateVarEnv name val rest of
        Right env -> Right $ frame : env
        Left err2 -> Left err2

-- mutateFrame checks both that the type is correct, and
-- whether the variable exists. If either case doesn't hold, 
-- an error is returned. 
mutateFrame :: VariableName -> Value -> Frame -> Either RuntimeError Frame
mutateFrame name _ []  = Left $ UnboundVariable name
mutateFrame name val ((var, (t, v)) : rest) = 
  case name == var of
    True -> 
      case val `isType` t of
        True -> Right $ (var, (t, val)) : rest
        False -> Left $ TypeMismatch val t
    False ->
      case mutateFrame name val rest of
        Right f -> Right $ (var, (t, v)) : f
        Left err -> Left err

-- Declaration of a variable. Is declared in the topmost frame.
declareVarEnv :: DeclType -> VariableName -> Value -> VariableEnvironment -> 
                 Either RuntimeError VariableEnvironment
declareVarEnv _ _ _ [] = Left $ InterpreterError "Oops, no frames found!"
declareVarEnv t name val (fr : rest) = 
  case declareFrame t name val fr of
    Right newFrame -> Right $ newFrame : rest
    Left err -> Left err

-- Checks if given variable already exists in the frame. If not,
-- add the vairable to the frame, bound to the value (and type).
declareFrame :: DeclType -> VariableName -> Value -> Frame -> Either RuntimeError Frame
declareFrame t name val fr = 
  case lookup name fr of
    Just _ -> Left $ RedeclarationError name 
    Nothing -> 
      case val `isType` t of
        True -> Right $ (name, (t, val)) : fr
        False -> Left $ TypeMismatch val t

-- Given parameters and arguments, initiates a new environment.
-- Handles cases of too few/too many arguments.
initiateVarEnv :: ProcedureParameters -> ProcedureArguments -> 
                  Either RuntimeError VariableEnvironment
initiateVarEnv [] [] = Right [[]]
initiateVarEnv [] (_ : _) = Left $ BadArgument "Supplied too many arguments!"
initiateVarEnv (_ : _) [] = Left $ BadArgument "Supplied too few arguments!"
initiateVarEnv ((t, var) : params) (val : args) = 
  case initiateVarEnv params args of
    Right varEnv -> declareVarEnv t var val varEnv
    Left err -> Left err

-- Recursively tries to find the nearest value of the 
-- variable "name".
findVar :: VariableName -> VariableEnvironment -> Maybe Value
findVar _ [] = Nothing
findVar name (f : rest) = 
  case (lookup name f) of
    Just (_, v) -> Just v
    Nothing -> findVar name rest
------------------------ END ENVIRONMENTS ------------------------

--------------------------- OBJB MONAD ---------------------------

type Runtime a   = VariableEnvironment -> (Either RuntimeError a, Output)
newtype ObjB a = ObjB {run :: Runtime a}

-- The same monad from Boa.
instance Monad ObjB where
  return a = ObjB $ \_ -> (Right a, []) 
  bp >>= f = ObjB $ \env ->
    case run bp env of
      (Left err, output) -> (Left err, output)
      (Right a , output) -> second (output++) (run (f a) env)

instance Functor ObjB where
  fmap = liftM
instance Applicative ObjB where
  pure = return; (<*>) = ap

-- abort re simply evaluates to a ObjB,
-- where given any Environment, gives out 
-- a RuntimeError.
abort :: RuntimeError -> ObjB a
abort re = ObjB $ \_ -> (Left re, [])

-- look n gives a ObjB that returns a
-- value if n is bound in a given Environment.
-- Otherwise, an error is produced. 
look :: VariableName -> ObjB Value
look name = 
  ObjB $ \varEnv -> 
    case findVar name varEnv of
      Just v -> (Right $ v, []) 
      Nothing -> (Left $ UnboundVariable name, [])

-- Constructs a ObjB monad, which simply outputs some string.
output :: String -> ObjB ()
output s = ObjB $ \_ -> (Right (), [s])

-- A function which outputs a new ObjB monad,
-- where the type of val is checked before executing
-- the next monad. Would've liked to use this more.
checkType :: Value -> DeclType -> (ObjB a -> ObjB a)
checkType val t = \o ->
  ObjB $ \varEnv ->
    case val `isType` t of
      True -> run o varEnv
      False -> run (abort $ TypeMismatch val t) varEnv

-- Adds a new frame to some envrionment.
expandEnv :: ObjB a -> ObjB a
expandEnv o = ObjB $ \varEnv -> run o ([] : varEnv)

-- Removes the topmost frame of some environment.
shrinkEnv :: ObjB a -> ObjB a
shrinkEnv o = 
  ObjB $ \varEnv ->
    case varEnv of 
      (_ : rest) -> run o rest
      [] -> run (abort $ InterpreterError "No frames to remove!") varEnv

-- Takes a ObjB and tries to add the variable "name" to the
-- given environment.
declaration :: DeclType -> VariableName -> Value -> (ObjB a -> ObjB a)
declaration t name val = 
  \b ->
    (ObjB $ \varEnv -> 
      case declareVarEnv t name val varEnv of
        Right newVarEnv -> run b newVarEnv
        Left err -> run (abort err) varEnv)

-- Takes a ObjB and tries to mutate the variable "name" in the
-- given environment.
mutation :: VariableName -> Value -> (ObjB a -> ObjB a)
mutation name val = 
  \b -> (ObjB $ \varEnv ->
    case mutateVarEnv name val varEnv of
      Right newVarEnv -> run b newVarEnv
      Left err -> run (abort err) varEnv)

------------------------- END OBJB MONAD -------------------------

------------------------ HELPER FUNCTIONS ------------------------

-- Just a function to fetch the truth value
-- of a VBoolean value. We never check the truth
-- value of anything else (without aborting), so
-- no need for something more fancy. 
truthy :: Value -> Bool
truthy val = 
  case val of
    VBoolean b -> b
    _ -> False

-- Compare a value to a type.
isType :: Value -> DeclType -> Bool
isType val t =
  case val of 
    VNumber _ -> t == TNumber
    VBoolean _ -> t == TBoolean
    VNull -> t == TVoid

-- For printing
valueToString :: Value -> String
valueToString v =
  case v of
    VNull -> "null"
    VBoolean b -> show b
    VNumber i -> show i

---------------------- END HELPER FUNCTIONS ----------------------

-------------------- EVALUATION AND EXECUTION --------------------

-- Could've probably used checkType in some way here instead,
-- but decided not to.
operate :: OperationSymbol -> Value -> Value -> ObjB Value
operate op val1 val2 = 
  case (val1, val2) of
    (VNumber x, VNumber y) ->
      case op of
        Eq -> return $ VBoolean $ x == y
        Plus -> return $ VNumber $ x + y
        Minus -> return $ VNumber $ x - y
        Mult -> return $ VNumber $ x * y
        Div -> if y /= 0 then return $ VNumber $ x `div` y
                         else abort DivisionByZero
        Less -> return $ VBoolean $ x < y
        _ -> abort $ BadOperation val1 op val2
    (VBoolean p, VBoolean q) ->
      case op of
        Eq -> return $ VBoolean $ p == q
        And -> return $ VBoolean $ p && q
        Or -> return $ VBoolean $ p || q
        _ -> abort $ BadOperation val1 op val2
    (VNull, VNull) ->
      case op of 
        Eq -> return $ VBoolean $ True
        _ -> abort $ BadOperation VNull op VNull
    _ -> abort $ BadOperands val1 val2

-- The procedures (or program) is passed in with
-- an expression. Not ideal(?), but it works.
eval :: Expression -> ProcedureEnvironment -> ObjB Value
eval (Operation op e1 e2) procEnv =
  do v1 <- eval e1 procEnv
     v2 <- eval e2 procEnv
     operate op v1 v2
eval (Constant val) _ = return val
eval (Variable name) _ = look name
-- The semantics specifically state that only booleans
-- can be negated. As such, I assume only booleans have
-- a truth value.
eval (Not exp) procEnv = 
  do  v1 <- eval exp procEnv
      checkType v1 TBoolean $ return $ VBoolean $ not $ truthy v1
-- This looks worse than it is, ok? It's just checking things
-- in order.
eval (Call f args) procEnv =
  do listOfValues <- ((flip eval) procEnv) `mapM` args
     case f of -- check for built-in procedures, add more in this case.
       "print" -> (output $ unwords (valueToString <$> listOfValues)) >> return VNull
       _ ->
        case lookup f procEnv of -- check if f is defined
          Nothing -> abort $ BadProcedure f
          Just (t, p, b) -> 
            case initiateVarEnv p listOfValues of -- try to bind arguments to parameters
              Left err -> abort err
              Right varEnv ->
                case run (exec b procEnv VNull) varEnv of -- try to execute a procedure call
                  (Right result, out) -> 
                    checkType result t $ foldr (\str -> (output str >>)) (return result) out 
                  (Left err, out) -> foldr (\str -> (output str >>)) (abort err) out

-- Same as eval, we pass the procedures (program) as an input.
-- Additionally, we have a parameter "last", which is just the
-- previous evaluated statement's value. It is used to return
-- the final statement in a sequence of statements, if no
-- explicit return statement is encountered. Not ideal either,
-- but whatever.
exec :: Statements -> ProcedureEnvironment -> Value -> ObjB Value
exec [] _ last = return last
exec (Define t var exp : body) procEnv _ =
  do val <- eval exp procEnv
     declaration t var val (exec body procEnv val)
exec (Mutate var exp : body) procEnv _ =
  do val <- eval exp procEnv
     mutation var val (exec body procEnv val)
exec (Execute exp : body) procEnv _ =
  do val <- eval exp procEnv
     exec body procEnv val
exec (If exp s1 s2 : body) procEnv last =
  do cond <- eval exp procEnv
     checkType cond TBoolean $ 
      case truthy cond of
        True  -> exec (s1 : body) procEnv last -- dummy last
        False -> exec (s2 : body) procEnv last -- dummy last
exec (While exp s1 : body) procEnv last =
  do cond <- eval exp procEnv
     checkType cond TBoolean $
        case truthy cond of
          True -> exec (s1 : (While exp s1) : body) procEnv last -- dummy last
          False -> exec body procEnv VNull
-- When reading a braced statement, we simply
-- add the statements in the braces to the statements of
-- the procedure call, and expand the environment with a new frame.
-- Since these statements always end with an EndBrace statement,
-- we know when to shrink the environment again.
exec (Braced statements : body) procEnv last =
  expandEnv $ exec (statements ++ body) procEnv last -- dummy last
-- Reading an EndBrace statement means we've exited
-- a braced section, and should remove the topmost frame.
exec (EndBrace : body) procEnv last = shrinkEnv $ exec body procEnv last -- NOT dummy last
exec (Return exp : _) procEnv _ =
  do val <- eval exp procEnv
     return val
exec (Skip : body) procEnv _ = exec body procEnv VNull
 
-- To execute a program, we run the ObjB monad
-- computed by executing the call on the main procedure
-- with the empty Environment.
execute :: Program -> ProcedureInput -> (Either RuntimeError Value, Output)
execute prog input = 
  let procEnv = ((\(Proc t name param body) -> (name, (t, param, body))) <$> prog) in
    run (eval (Call "main" input) procEnv) [[]]
------------------ END EVALUATION AND EXECUTION ------------------