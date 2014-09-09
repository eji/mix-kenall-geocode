-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:
module SpecHelper
( 
  module Test.Hspec
, shouldParse
, assertNotParse
)
where

import Test.Hspec
import Test.HUnit.Base
import qualified Text.Parsec.Error as P

shouldParse :: (Eq a, Show a) => Either P.ParseError a -> a -> Assertion
shouldParse expect result = 
  case expect of
    Right r -> r `shouldBe` result
    Left _ -> assertFailure "parse failed"

assertNotParse :: Either P.ParseError a -> Assertion
assertNotParse x = 
  case x of
    Right _ -> assertFailure "should not parse"
    Left _ -> assertBool "succ" True
