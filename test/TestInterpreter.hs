module TestInterpreter where

import Src.Interpreter
import Test.HUnit

premise =
  [ SLiteral $ Literal "human" [Atom "socrates"], -- human(socrates).
    SLiteral $ Literal "human" [Atom "iwakura"], -- human(iwakura).
    SLiteral $ Literal "name" [Atom "iwakura", Atom "lain"], -- human(iwakura).
    Sentence (Literal "die" [Variable "X"]) (CLiteral $ Literal "human" [Variable "X"]) -- die(X) :- human(X).
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

test2 =
  TestCase
    ( assertEqual
        ""
        [ (True, [(Variable "Y", Atom "socrates")]),
          (True, [(Variable "Y", Atom "iwakura")])
        ]
        (searchCondition premise (Literal "die" [Variable "Y"]))
    )

test3 =
  TestCase
    ( assertEqual
        ""
        [(True, [(Atom "socrates", Atom "socrates")])]
        (searchCondition premise (Literal "die" [Atom "socrates"]))
    )

tests = TestList [TestLabel "1" test1, TestLabel "2" test2, TestLabel "3" test3]

main = runTestTT tests
