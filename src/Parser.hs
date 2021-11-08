{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser where

import Data.Type.Coercion (sym)
import DataType
import Text.ParserCombinators.Parsec

symbol = "():-,."

atomVal (Atom x) = x

variable = do
  i <- upper
  x <- many (noneOf symbol)
  return $ Variable $ i : x

atom = do
  i <- lower
  x <- many (noneOf symbol)
  return $ Atom $ i : x

term = variable <|> atom

termList = do
  char '('
  x <- many $ do
    try
      ( do
          x <- term
          char ','
          return x
      )
      <|> term

  char ')'
  return x

literal = do
  name <- atom
  Literal (atomVal name) <$> termList

clauseListToAnd [c] = CLiteral c
clauseListToAnd (c : cs) = And c (clauseListToAnd cs)

cAnd = do
  x <- many $ do
    try
      ( do
          x <- literal
          char ','
          return x
      )
      <|> literal

  return $ clauseListToAnd x

clause = cAnd

lSentence = do
  l <- literal
  char '.'
  return $ SLiteral l

sentence' = do
  l <- literal
  try
    ( do
        string ":-"
        c <- try clause
        char '.'
        return $ Sentence l c
    )
    <|> do
      char '.'
      return $ SLiteral l

sentence = sentence' <|> lSentence

sentences = many sentence
