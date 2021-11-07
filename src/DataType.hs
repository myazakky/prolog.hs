module DataType where

data Term = Variable String | Atom String deriving (Show)

data Literal = Literal
  { literalName :: String,
    literalArgs :: [Term]
  }
  deriving (Show)

data Clause = CLiteral Literal | And Literal Clause deriving (Show)

data Sentence = SLiteral Literal | Sentence Literal Clause deriving (Show)
