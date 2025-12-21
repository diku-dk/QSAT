module Eval_Tests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "evalTerm"
    [ 
        testCase "test" $ 1 @?= 1 
    ]
