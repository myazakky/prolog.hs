module TestParser where

import DataType
import Parser
import Text.Parsec as Pc

main = do
  Pc.parseTest sentence "human(socrates)."
  Pc.parseTest sentence "die(X):-human(X)."
  Pc.parseTest premise "human(socrates).die(X):-human(X)."
