module DataType where

import Data.Foldable (Foldable (fold))

data Term = Variable String | Atom String

instance Show Term where
  show (Variable x) = x
  show (Atom x) = x

instance Eq Term where
  (==) (Atom a1) (Atom a2) = a1 == a2
  (==) _ _ = True

data Literal = Literal
  { literalName :: String,
    literalArgs :: [Term]
  }

instance Show Literal where
  show l = literalName l ++ show (literalArgs l)

type Assigment = (Term, Term)

type Condition = (Bool, [Assigment])

show' :: [Condition] -> String
show' [] = "False"
show' c = concat $ map (show . snd) c

data Clause = CLiteral Literal | And Literal Clause deriving (Show)

data Sentence = SLiteral Literal | Sentence Literal Clause deriving (Show)
