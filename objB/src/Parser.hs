module Parser (ParseError, parseString, parseArguments) where

import Syntax
import Text.ParserCombinators.Parsec
import Data.Char (isDigit)
import Text.Parsec.Char (endOfLine)

-- ########################################################
-- Main parsers, controlling precedence and associativity
-- ########################################################
-- Parses the arguments when running Main.hs
parseArguments :: String -> Either ParseError ProcedureInput
parseArguments = parse (between (symbol "(") (symbol ")") exprz) ""

parseString :: String -> Either ParseError Program
parseString = parse (skipSpaces >> (procedures <* eof)) ""

-- A program is just sequence of procedures.
procedures :: Parser Program
procedures = many procedure

procedure :: Parser Procedure
procedure =
  do t <- declType
     id <- declIdent
     params <- parameters
     body <- between (symbol "{") (symbol "}") statements
     return $ Proc t id params body

parameters :: Parser ProcedureParameters
parameters = between (symbol "(") (symbol ")") (parameter `sepBy` (symbol ","))
  where
    parameter =
      do t <- declType
         id <- declIdent
         return (t, id)

statements :: Parser [Statement]
statements = statement `sepBy` (symbol ";")

statement :: Parser Statement
statement = 
  -- We attach an EndBrace statement at the
  -- end of the statement list as a way for the
  -- interpreter to know when the statements from
  -- a braced statement has been executed. 
  -- More on this in the interpreter.
  choice [ Braced <$> ((flip (++)) [EndBrace]) <$> 
            (between (symbol "{") (symbol "}") statements)
         , try declaration
         , try mutation
         , try while
         , try ifThenElse
         , try returnStatement
         , try $ Execute <$> expr1
         , (skipSpaces >> return Skip) 
         ]
  where
    while = 
      do symbolWS "while"
         cond <- expr1
         body <- statement
         return $ While cond body
    ifThenElse =
      do symbolWS "if"
         cond <- expr1
         symbolWS "then"
         s1 <- statement
         string "else" 
         -- Either we read one or more whitespaces and then a statement, 
         -- or we'll read the empty statement. Note the use of try, to
         -- avoid something like 'elseBAD' being potentially parsed as 'else Skip'
         -- but rather 'else Skip BAD'. Not ideal, but whatever.
         s2 <- (try (skipSpaces1 >> statement)) <|> (skipSpaces >> return Skip)
         return $ If cond s1 s2
    returnStatement =
      do symbolWS "return" -- Not needed here though, as return require an expression.
         e1 <- expr1
         return $ Return e1
    declaration =
      do t <- declType
         id <- declIdent
         symbol "="
         ex <- expr1
         return $ Define t id ex
    mutation =
      do id <- declIdent
         symbol "="
         ex <- expr1
         return $ Mutate id ex

expr1 :: Parser Expression
expr1 = try notExp <|> expr2 
  where
    notExp :: Parser Expression
    notExp = 
      do symbol "!"  -- Assume whitespaces between "!" and an 
         ex <- expr1 -- expression is fine. If not, can use string instead.
         return $ Not ex

-- I use "chainl1" to get around left recursion.
-- This enables us to parse expressions as
-- e1 && e2 || e3.
expr2 :: Parser Expression
expr2 = try boolOperation <|> expr3
  where 
    boolOperation = expr3 `chainl1` (Operation <$> boolOps)

expr3 :: Parser Expression
expr3 = try compareOperation <|> expr4
  where 
    compareOperation =
      do ex1 <- expr4
         op <- compOps
         ex2 <- expr4
         return $ op ex1 ex2

expr4 :: Parser Expression
expr4 = try addOperation <|> expr5
  where 
    addOperation = expr5 `chainl1` (Operation <$> addOps)

expr5 :: Parser Expression
expr5 = try multOperation <|> expr6
  where
    multOperation = expr6 `chainl1` (Operation <$> multOps)
    
expr6 :: Parser Expression
expr6 = 
  choice [between (symbol "(") (symbol ")") expr1,
          try functionCall,
          try $ Variable <$> ident,
          try $ Constant <$> val]

-- #################################################
-- Operation symbol parsers
-- #################################################
multOps :: Parser OperationSymbol
multOps = 
  choice [symbol "*" >> return (Mult),
          symbol "/" >> return (Div)]

addOps :: Parser OperationSymbol
addOps = 
  choice [symbol "+" >> return (Plus),
          symbol "-" >> return (Minus)]

compOps :: Parser (Expression -> Expression -> Expression)
compOps = 
  choice [symbol "==" >> return (Operation Eq),
          symbol "<" >> return (Operation Less)]

boolOps :: Parser OperationSymbol
boolOps =
  choice [symbol "&&" >> return (And),
          symbol "||" >> return (Or)]

-- #################################################
-- Atomic parsers
-- #################################################
val :: Parser Value
val = numConst <|> keyword

numConst :: Parser Value
numConst = VNumber <$>
  do num <- ((string "-" >> ((0 -) <$> zeroOrNum)) <|> zeroOrNum) 
     return num
  where
    zeroOrNum :: Parser Integer
    zeroOrNum = (symbol "0" >> return 0) <|> pNum

-- taken from the file "MyFirstParser.hs" 
-- on the course page, and slightly edited.
pNum :: Parser Integer
pNum = lexeme $
  do ds <- many1 (satisfy isDigit)
     notFollowedBy letter
     return $ read ds

keyword :: Parser Value
keyword = do symbol "null"
             return $ VNull
      <|> do symbol "true"    
             return $ VBoolean True 
      <|> do symbol "false"
             return $ VBoolean False

-- #################################################
-- Non-atomic parsers
-- #################################################
functionCall :: Parser Expression
functionCall = 
  do id <- ident
     expList <- between (symbol "(") (symbol ")") exprz
     return $ Call id expList

exprz :: Parser [Expression]
exprz = expr1 `sepBy` (symbol ",")

-- Keeping the same identifiers from Boa
ident :: Parser VariableName
ident = lexeme $ 
  do first <- (letter <|> char '_')
     rest  <- many (alphaNum <|> char '_')
     case reservedWord (first : rest) of
      False -> return (first : rest)
      True  -> unexpected (first : rest)

-- Used when declaring/mutating variables
declIdent :: Parser VariableName
declIdent =
  do name <- ident
     case reservedIdent name of
      False -> return name
      True -> unexpected name

-- Words reserved in Objective B. Should not
-- be able to try to name or refer to variables
-- matching these strings.
reservedWord :: String -> Bool
reservedWord = 
  flip elem $ ["null", "true", "false", "if", "then", 
               "else", "while", "return", "integer",
               "boolean", "void"]

-- Identifiers reserved in Objective B. These
-- cannot be overwritten, but referred to.
reservedIdent :: String -> Bool
reservedIdent = flip elem $ ["print"]

-- Every time we read a type, there needs to be a whitespace after.
declType :: Parser DeclType
declType = 
  choice [symbolWS "integer" >> return (TNumber), 
          symbolWS "boolean" >> return (TBoolean), 
          symbolWS "void"    >> return (TVoid)]

-- #################################################
-- Basic/auxillary parsers
-- #################################################
-- Skip 0 or more spaces after applying parser p
lexeme :: Parser a -> Parser a
lexeme p =
  do a <- p
     skipSpaces
     return a

-- Skip 1 or more spaces after applying parser p
lexemeWS :: Parser a -> Parser a
lexemeWS p =
  do a <- p
     skipSpaces1
     return a

-- Read some symbol, then skip 0 or more spaces
symbol :: String -> Parser ()
symbol s = lexeme $ string s >> return ()

-- Read some symbol, then skip 1 or more spaces
symbolWS :: String -> Parser ()
symbolWS s = lexemeWS $ string s >> return ()

-- I include comments, in the same style as Boa, i.e. '#'.
-- Parses a string beginning with #, until a new line is encountered, 
-- i.e. skips the rest of a line if it is a comment.
-- Since manyTill fails if endOfLine is never used, (many anyChar)
-- is used to read the rest of the program as a comment.
-- Afterwards, we skip any whitespaces.
comment :: Parser ()
comment = (string "#") >> 
          (((try $ manyTill anyChar (try endOfLine))) <|> (many anyChar)) >> 
          spaces

-- Skips multiple lines of comments, and any whitespaces afterwards.
skipComments :: Parser ()
skipComments = skipMany comment

-- Skip 0 or more spaces
skipSpaces :: Parser ()
skipSpaces = skipMany space >> skipComments

-- Skip 1 or more spaces
skipSpaces1 :: Parser ()
skipSpaces1 = skipMany1 space >> skipComments