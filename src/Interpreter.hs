module Interpreter where

import Data.List (intersectBy)
import DataType
import Text.Show (Show)

type Assigment = (Term, Term)

type Condition = (Bool, [Assigment])

canTakeTerm :: [Assigment] -> Term -> Bool
canTakeTerm as t = any (\a -> fst a === t) as

takeTerm :: [Assigment] -> Term -> Term
takeTerm as t = snd $ head $ filter (\a -> fst a === t) as

isVariable :: Term -> Bool
isVariable (Variable _) = True
isVariable _ = False

areConflict :: Condition -> Condition -> Bool
areConflict (_, a1) (_, a2) = any (\a -> canTakeTerm a2 (fst a) && takeTerm a2 (fst a) /= snd a) a1

arentConflict c1 c2 = not (areConflict c1 c2)

substitute :: Literal -> [Assigment] -> Literal
substitute l as = Literal (literalName l) (map (\t -> if canTakeTerm as t then takeTerm as t else t) (literalArgs l))

instance Eq Term where
  (==) (Atom a1) (Atom a2) = a1 == a2
  (==) _ _ = True

(===) (Atom a1) (Atom a2) = a1 == a2
(===) (Variable a1) (Variable a2) = a1 == a2
(===) _ _ = False

(==^) :: Literal -> Literal -> Bool
(==^) l1 l2 =
  literalName l1 == literalName l2
    && literalArgs l1 == literalArgs l2

goal :: Sentence -> Literal
goal (SLiteral l) = l
goal (Sentence l _) = l

findReasons :: [Sentence] -> Literal -> [Sentence]
findReasons s l = filter ((l ==^) . goal) s

transferVariableName :: Condition -> [Assigment] -> Condition
transferVariableName c a =
  (fst c, map (\a' -> if canTakeTerm a (fst a') then (takeTerm a (fst a'), snd a') else a') (snd c))

searchCondition'' :: [Sentence] -> [Assigment] -> Clause -> [Condition]
searchCondition'' premise as (CLiteral l) = searchCondition premise (substitute l as)
searchCondition'' premise as (And l c) =
  let lcondition = searchCondition premise (substitute l as)
   in intersectBy arentConflict lcondition (searchCondition'' premise as c)

elem' :: Term -> [Term] -> Bool
elem' t = any (=== t)

removeNoise' :: [Assigment] -> [Term] -> [Assigment]
removeNoise' as args =
  let wanted = filter isVariable args
   in filter (\a -> (fst a `elem'` wanted) || not (isVariable (fst a) || fst a === snd a)) as

removeNoise :: [Condition] -> [Term] -> [Condition]
removeNoise cs ts = map (\c -> (fst c, removeNoise' (snd c) ts)) cs

searchCondition' :: [Sentence] -> Literal -> Sentence -> [Condition]
searchCondition' premise l1 (SLiteral l2) = removeNoise [(True, zip (literalArgs l1) (literalArgs l2))] (literalArgs l1)
searchCondition' premise l1 (Sentence l2 c) =
  let assigments = zip (literalArgs l2) (literalArgs l1)
      conditions = searchCondition'' premise assigments c
   in if null conditions
        then []
        else removeNoise (map (`transferVariableName` assigments) conditions) (literalArgs l1)

searchCondition :: [Sentence] -> Literal -> [Condition]
searchCondition premise proposition = filter fst $ concatMap (searchCondition' premise proposition) (findReasons premise proposition)
