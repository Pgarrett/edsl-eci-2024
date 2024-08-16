{-# LANGUAGE GADTs, KindSignatures #-}

import Test.HUnit

data Expr:: * -> * where
    Val     :: Int -> Expr Int
    Eq      :: Expr Int -> Expr Int -> Expr Bool
    Lt      :: Expr Int -> Expr Int -> Expr Bool
    Not     :: Expr Bool -> Expr Bool
    And     :: Expr Bool -> Expr Bool -> Expr Bool
    Or      :: Expr Bool -> Expr Bool -> Expr Bool

eval :: Expr t -> t
eval (Val n)    = n
eval (Eq e1 e2) = (eval e1) == (eval e2)
eval (Lt e1 e2) = (eval e1) < (eval e2)
eval (Not e1)   = not (eval e1)
eval (And e1 e2) = (eval e1) && (eval e2)
eval (Or e1 e2) = (eval e1) || (eval e2)


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
    eval (Val 3) ~=? 3    
    ]

testSameValuesAreEqual = test [
    eval (Eq (Val 3) (Val 3)) ~=? True
    ]

testDifferentValuesAreNotEqual = test [
    eval (Eq (Val 3) (Val 4)) ~=? False
    ]

test3IsLowerThan4 = test [
    eval (Lt (Val 3) (Val 4)) ~=? True
    ]

test5IsNotLowerThan4 = test [
    eval (Lt (Val 5) (Val 4)) ~=? False
    ]

testNot3IsLowerThan4ReturnsFalse = test [
    eval (Not (Lt (Val 3) (Val 4))) ~=? False
    ]

testNot5IsLowerThan4ReturnsTrue = test [
    eval (Not (Lt (Val 5) (Val 4))) ~=? True
    ]

testAndTrueFalseReturnsFalse = test [
    eval (And (Lt (Val 3) (Val 4)) (Lt (Val 5) (Val 4))) ~=? False
    ]

testAndTrueTrueReturnsTrue = test [
    eval (And (Lt (Val 3) (Val 4)) (Lt (Val 2) (Val 4))) ~=? True
    ]

testOrTrueFalseReturnsTrue = test [
    eval (Or (Lt (Val 3) (Val 4)) (Lt (Val 5) (Val 4))) ~=? True
    ]

testOrFalseFalseReturnsFalse = test [
    eval (Or (Lt (Val 7) (Val 4)) (Lt (Val 4) (Val 4))) ~=? False
    ]