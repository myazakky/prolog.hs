module TestInterpreter where

import Src.Interpreter
import Test.HUnit

premise =
  [ SLiteral $ Literal "human" [Atom "warui"], -- human(warui).
    SLiteral $ Literal "human" [Atom "iwakura"], -- human(iwakura).
    SLiteral $ Literal "name" [Atom "iwakura", Atom "lain"], -- name(iwakura, lain)
    SLiteral $ Literal "sinner" [Atom "warui"], -- sinner(warui).
    Sentence (Literal "die" [Variable "X"]) (CLiteral $ Literal "human" [Variable "X"]), -- die(X) :- human(X).
    Sentence
      (Literal "go_to_hell" [Variable "X"])
      ( And
          (Literal "human" [Variable "X"])
          (CLiteral $ Literal "sinner" [Variable "X"])
      ), -- go_to_hell(X) :- die(X) sinner(X).
    Sentence
      (Literal "exist" [Variable "X"])
      ( And
          (Literal "name" [Variable "Y", Variable "X"])
          (CLiteral $ Literal "human" [Variable "Y"])
      ) -- exist(X) :- name(X, Y) human(Y).
  ]

test1 =
  TestCase
    ( assertEqual
        ""
        [ (True, [(Variable "Y", Atom "warui")]),
          (True, [(Variable "Y", Atom "iwakura")])
        ]
        (searchCondition premise (Literal "human" [Variable "Y"]))
    )

test2 =
  TestCase
    ( assertEqual
        ""
        [ (True, [(Variable "Y", Atom "warui")]),
          (True, [(Variable "Y", Atom "iwakura")])
        ]
        (searchCondition premise (Literal "die" [Variable "Y"]))
    )

test3 =
  TestCase
    ( assertEqual
        ""
        [(True, [(Variable "X", Atom "warui")])]
        (searchCondition premise (Literal "go_to_hell" [Variable "X"]))
    )

test4 =
  TestCase
    ( assertEqual
        ""
        [(True, [])]
        (searchCondition premise (Literal "exist" [Atom "lain"]))
    )

test5 =
  TestCase
    ( assertEqual
        ""
        [(True, [(Variable "X", Atom "iwakura")])]
        (searchCondition premise (Literal "name" [Variable "X", Atom "lain"]))
    )

-- (substitute (Literal "name" [Variable "Y", Variable "X"]) [(Variable "X", Atom "lain")])

tests = TestList [TestLabel "1" test1, TestLabel "2" test2, TestLabel "3" test3, TestLabel "4" test4, TestLabel "5" test5]

main = runTestTT tests
