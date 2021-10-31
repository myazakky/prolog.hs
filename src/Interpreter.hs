module Src.Interpreter where

import Text.Show (Show)

data Term = Variable String | Atom String deriving (Show)

type Assigment = (Term, Term)

type Condition = (Bool, [Assigment])

data Literal = Literal
  { literalName :: String,
    literalArgs :: [Term]
  }
  deriving (Show)

instance Eq Term where
  (==) (Variable _) _ = True
  (==) (Atom a1) (Atom a2) = a1 == a2
  (==) _ _ = False

(==^) :: Literal -> Literal -> Bool
(==^) l1 l2 =
  literalName l1 == literalName l2
    && literalArgs l1 == literalArgs l2

data Clause = CLiteral Literal | And Literal Clause deriving (Show)

data Sentence = SLiteral Literal | Sentence Literal Clause deriving (Show)

goal :: Sentence -> Literal
goal (SLiteral l) = l
goal (Sentence l _) = l

findReasons :: [Sentence] -> Literal -> [Sentence]
findReasons s l = filter ((l ==^) . goal) s

searchCondition' :: [Sentence] -> Literal -> Sentence -> [Condition]
searchCondition' premise l1 (SLiteral l2) = [(True, zip (literalArgs l1) (literalArgs l2))]
searchCondition' _ _ _ = []

searchCondition :: [Sentence] -> Literal -> [Condition]
searchCondition premise proposition = concatMap (searchCondition' premise proposition) (findReasons premise proposition)
