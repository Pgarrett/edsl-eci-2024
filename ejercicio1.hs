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

-- tests :: IO Counts
-- tests = do runTestTT allTests

-- allTests = test [
--     "testValSavesInput" ~: testValSavesInput    
--     ]

-- v3 :: Eval Int
-- v3 = val 3

-- testValSavesInput = test [
--     v3 ~=? 3
--     ]


-- tests :: IO Counts
-- tests = do runTestTT allTests

-- allTests = test [
--   "ejercicio1" ~: testsEj1,
--   "ejercicio2" ~: testsEj2,
--   "ejercicio3" ~: testsEj3,
--   "ejercicio4" ~: testsEj4,
--   "ejercicio5" ~: testsEj5,
--   "ejercicio6" ~: testsEj6,
--   "auxiliar-apareceIgualCantidadDeVeces" ~: testsApareceIgualCantidadDeVeces,
--   "auxiliar-contarOcurrenciasTupla" ~: testsContarOcurrenciasTupla
--   ]