-- Abstract syntax definitions for Objective B.
-- Inspired by the Boa language.

module Syntax where

type Program = [Procedure]

data Procedure =
  Proc DeclType ProcedureName ProcedureParameters ProcedureBody 
  deriving (Eq, Read)
instance Show Procedure where
  show (Proc t id params body) = 
   "Proc " ++ show t ++ " " ++ show id ++ " " ++ show params ++ 
   "\n   " ++ show body ++ "\n\n" 

data DeclType =
    TVoid
  | TBoolean
  | TNumber
  deriving (Eq, Show, Read)

type VariableName      = String
type ProcedureName      = String
type ProcedureInput     = [Expression]
type ProcedureArguments = [Value]
type ProcedureParameters = [(DeclType, VariableName)]
type ProcedureBody = Statements

-- Statements (ProcedureBody) is a list of statements.
-- If there is a braced statement in a body, it will be
-- represented as [s1', ..., Braced [s1,...,sn, EndBrace], ..., sn'], where
-- each s1/s1' can also be Braced [...].
type Statements = [Statement]

data Statement =
    Define DeclType VariableName Expression
  | Mutate VariableName Expression
  | Execute Expression
  | If Expression Statement Statement
  | While Expression Statement
  | Return Expression
  | Braced Statements
  | Skip
  | EndBrace -- ITS A BIT DUMMY DUMB DUMB, I VISCOUS BRAIN. Basically at the end of
             -- the Statements list of a Braced statement, there is always an EndBrace.
             -- This is to make things in the interpreter a bit easier.
  deriving (Eq, Show, Read)

data Expression =
    Constant  Value
  | Variable  VariableName
  | Operation OperationSymbol Expression Expression
  | Not Expression
  | Call ProcedureName ProcedureInput
  deriving (Eq, Show, Read)

data OperationSymbol =
    Plus
  | Minus
  | Mult
  | Div
  | Eq
  | Less
  | And 
  | Or
  deriving (Eq, Show, Read)

data Value =
    VNull
  | VBoolean Bool
  | VNumber Integer
  deriving (Eq, Show, Read)