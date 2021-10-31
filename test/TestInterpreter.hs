module TestInterpreter where

import Src.Interpreter
import Test.HUnit

premise =
  [ SLiteral $ Literal "human" [Atom "socrates"], -- human(socrates).
    SLiteral $ Literal "human" [Atom "iwakura"] -- human(iwakura).
  ]

test1 =
  TestCase
    ( assertEqual
        ""
        [ (True, [(Variable "Y", Atom "socrates")]),
          (True, [(Variable "Y", Atom "iwakura")])
        ]
        (searchCondition premise (Literal "human" [Variable "Y"]))
    )

tests = TestList [TestLabel "1" test1]

main = runTestTT tests
