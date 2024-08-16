{-# LANGUAGE GADTs, KindSignatures #-}

import Test.HUnit

import qualified Prelude (not)
import Prelude hiding (not, and, or)

class SEExpr e where
    val    :: Int -> e Int
    eq     :: e Int -> e Int -> e Bool
    lt     :: e Int -> e Int -> e Bool
    not    :: e Bool -> e Bool
    and    :: e Bool -> e Bool -> e Bool
    or     :: e Bool -> e Bool -> e Bool

data SEPP a = SEP String
    deriving Show

instance SEExpr SEPP where
    val x           = SEP (show x)
    eq (SEP x) (SEP y)  = SEP ("(" ++ x ++ " == " ++ y ++ ")")
    lt (SEP x) (SEP y)  = SEP ("(" ++ x ++ " < " ++ y ++ ")")
    not (SEP b)       = SEP ("(~" ++ b ++ ")")
    and (SEP b) (SEP c) = SEP ("(" ++ b ++ " /\\ " ++ c ++ ")")
    or (SEP b) (SEP c)  = SEP ("(" ++ b ++ " \\/ " ++ c ++ ")")

fromEval (SEP x) = x


data DEExpr:: * -> * where
    Val     :: Int -> DEExpr Int
    Eq      :: DEExpr Int -> DEExpr Int -> DEExpr Bool
    Lt      :: DEExpr Int -> DEExpr Int -> DEExpr Bool
    Not     :: DEExpr Bool -> DEExpr Bool
    And     :: DEExpr Bool -> DEExpr Bool -> DEExpr Bool
    Or      :: DEExpr Bool -> DEExpr Bool -> DEExpr Bool

eval :: DEExpr t -> String
eval (Val n)    = show n
eval (Eq e1 e2) = "(" ++ (eval e1) ++ " == " ++ (eval e2) ++ ")"
eval (Lt e1 e2) = "(" ++ (eval e1) ++ " < " ++ (eval e2) ++ ")"
eval (Not e1)   = "(~" ++ (eval e1) ++ ")"
eval (And e1 e2) = "(" ++ (eval e1) ++ " /\\ " ++ (eval e2) ++ ")"
eval (Or e1 e2) = "(" ++ (eval e1) ++ " \\/ " ++ (eval e2) ++ ")"

tests :: IO Counts
tests = do runTestTT allTests

allTests = test [
    "testVal" ~: testVal,
    "testEqual" ~: testEqual,
    "testLowerThan" ~: testLowerThan,
    "testNot" ~: testNot,
    "testAnd" ~: testAnd,
    "testOr" ~: testOr
    ]


testVal = test [
    fromEval (val 3) ~=? "3",
    eval (Val 3) ~=? "3"
    ]

testEqual = test [
    fromEval (eq (val 3) (val 3)) ~=? "(3 == 3)",
    eval (Eq (Val 3) (Val 3)) ~=? "(3 == 3)"
    ]

testLowerThan = test [
    fromEval (lt (val 3) (val 4)) ~=? "(3 < 4)",
    eval (Lt (Val 3) (Val 4)) ~=? "(3 < 4)"
    ]

testNot = test [
    fromEval (not (lt (val 3) (val 4))) ~=? "(~(3 < 4))",
    eval (Not (Lt (Val 3) (Val 4))) ~=? "(~(3 < 4))"
    ]

testAnd = test [
    fromEval (and (lt (val 3) (val 4)) (lt (val 5) (val 4))) ~=? "((3 < 4) /\\ (5 < 4))",
    eval (And (Lt (Val 3) (Val 4)) (Lt (Val 5) (Val 4))) ~=? "((3 < 4) /\\ (5 < 4))"
    ]

testOr = test [
    fromEval (or (lt (val 3) (val 4)) (lt (val 5) (val 4))) ~=? "((3 < 4) \\/ (5 < 4))",
    eval (Or (Lt (Val 3) (Val 4)) (Lt (Val 5) (Val 4))) ~=? "((3 < 4) \\/ (5 < 4))"
    ]