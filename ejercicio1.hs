import Test.HUnit

import qualified Prelude (not)
import Prelude hiding (not, and, or)

class Expr e where
    val    :: Int -> e Int
    eq     :: e Int -> e Int -> e Bool
    lt     :: e Int -> e Int -> e Bool
    not    :: e Bool -> e Bool
    and    :: e Bool -> e Bool -> e Bool
    or     :: e Bool -> e Bool -> e Bool

data Eval t = E t
    deriving Show

instance Expr Eval where
    val x           = E x
    eq (E x) (E y)  = E (x == y)
    lt (E x) (E y)  = E (x < y)
    not (E b)       = E (Prelude.not b)
    and (E b) (E c) = E (b && c)
    or (E b) (E c)  = E (b || c)

fromEval (E x) = x

tests :: IO Counts
tests = do runTestTT allTests

allTests = test [
    "testValSavesInput" ~: testValSavesInput,
    "testSameValuesAreEqual" ~: testSameValuesAreEqual,
    "testDifferentValuesAreNotEqual" ~: testDifferentValuesAreNotEqual,
    "test3IsLowerThan4" ~: test3IsLowerThan4,
    "test5IsNotLowerThan4" ~: test5IsNotLowerThan4,
    "testNot3IsLowerThan4ReturnsFalse" ~: testNot3IsLowerThan4ReturnsFalse,
    "testNot5IsLowerThan4ReturnsTrue" ~: testNot5IsLowerThan4ReturnsTrue,
    "testAndTrueFalseReturnsFalse" ~: testAndTrueFalseReturnsFalse,
    "testAndTrueTrueReturnsTrue" ~: testAndTrueTrueReturnsTrue,
    "testOrTrueFalseReturnsTrue" ~: testOrTrueFalseReturnsTrue,
    "testOrFalseFalseReturnsFalse" ~: testOrFalseFalseReturnsFalse
    ]


testValSavesInput = test [
    fromEval (val 3) ~=? 3    
    ]

testSameValuesAreEqual = test [
    fromEval (eq (val 3) (val 3)) ~=? True
    ]

testDifferentValuesAreNotEqual = test [
    fromEval (eq (val 3) (val 4)) ~=? False
    ]

test3IsLowerThan4 = test [
    fromEval (lt (val 3) (val 4)) ~=? True
    ]

test5IsNotLowerThan4 = test [
    fromEval (lt (val 5) (val 4)) ~=? False
    ]

testNot3IsLowerThan4ReturnsFalse = test [
    fromEval (not (lt (val 3) (val 4))) ~=? False
    ]

testNot5IsLowerThan4ReturnsTrue = test [
    fromEval (not (lt (val 5) (val 4))) ~=? True
    ]

testAndTrueFalseReturnsFalse = test [
    fromEval (and (lt (val 3) (val 4)) (lt (val 5) (val 4))) ~=? False
    ]

testAndTrueTrueReturnsTrue = test [
    fromEval (and (lt (val 3) (val 4)) (lt (val 2) (val 4))) ~=? True
    ]

testOrTrueFalseReturnsTrue = test [
    fromEval (or (lt (val 3) (val 4)) (lt (val 5) (val 4))) ~=? True
    ]

testOrFalseFalseReturnsFalse = test [
    fromEval (or (lt (val 7) (val 4)) (lt (val 4) (val 4))) ~=? False
    ]
