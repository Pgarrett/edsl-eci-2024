import Prelude hiding ((<*>),(<$>),(<*),(*>),(<$))
import Data.Char

import Test.HUnit

data UProp :: * where
  Or  :: UProp -> UProp -> UProp
  And :: UProp -> UProp -> UProp
  Not :: UProp -> UProp
  Eq  :: UProp -> UProp -> UProp
  Lt  :: UProp -> UProp -> UProp
  Val :: Int -> UProp
  deriving (Show, Eq)


-- Primera parte toda copiada del archivo proporcionado de parsing
type Parser s a = [s] -> [(a, [s])]

pFail  :: Parser s a
pFail  = \cs -> []

pSucceed    :: a -> Parser s a
pSucceed a  =  \cs -> [(a,cs)]

pSym    :: Eq s => s -> Parser s s
pSym s  = \cs ->  case cs of
                    []         ->  []
                    (c : cs')  ->  if  c == s
                                       then [(c,cs')]
                                       else []

infixl 3 <|> 
infixl 4 <*> 
infixl 4 <* 
infixl 4 *> 
infixl 4 <$> 
infixl 4 <$ 

(<|>) ::  Parser s a -> Parser s a -> Parser s a
p <|> q = \cs -> case p cs of
                  [] -> q cs
                  res -> res -- para que el parser sea determinístico

(<*>) ::  Parser s (a -> b) -> Parser s a -> Parser s b
(p <*> q) cs = [ (f a, cs'')  |  (f , cs')   <- p cs
                              ,  (a , cs'')  <- q cs']

f <$> p  = pSucceed f <*> p

p <* q   = (\ x _ -> x) <$> p <*> q

p *> q   = (\ _ y -> y) <$> p <*> q

a <$ q   = pSucceed a <* q

p `opt` v = p <|> pSucceed v

pSat    :: (s -> Bool) -> Parser s s
pSat p  = \cs ->  case cs of
                   []         -> []
                   (c : cs')  -> if  p c
                                     then [(c,cs')]
                                     else []

-- p* (many)
pList :: Parser s a -> Parser s [a]
pList p =  (:) <$> p <*> pList p
           <|>
           pSucceed []


-- Implementacion. El parser no soporta espacios.
-- gramatica:
-- prop ::= tem "\/" prop | term
-- term ::= factor "/\" term | factor
-- factor ::= '~' prop | '(' prop ')' | '(' prop '=' prop ')' | '(' prop '<' prop ')' | N

-- Parser para números naturales (N)
pVal :: Parser Char UProp
pVal = Val . read <$> pList (pSat isDigit)

-- Parser para not (~p)
pNot :: Parser Char UProp
pNot = Not <$> (pSym '~' *> pProp)

-- Parser para paréntesis
pParens :: Parser Char UProp
pParens = pSym '(' *> pProp <* pSym ')'

-- Parser para Eq (p=p)
pEq :: Parser Char UProp
pEq = Eq <$> (pSym '(' *> pProp) <*> (pSym '=' *> pProp <* pSym ')')

-- Parser para Lt (p<p)
pLt :: Parser Char UProp
pLt = Lt <$> (pSym '(' *> pProp) <*> (pSym '<' *> pProp <* pSym ')')

-- Parser para factores, ordenado como la gramática. Como el parser es determinístico, si pongo pVal primero fallaría al intentar parsear algo como pFactor "3<4"
pFactor :: Parser Char UProp
pFactor = pNot <|> pParens <|> pEq <|> pLt <|> pVal

-- Parser para strings, parsea más fácil or y and
pString :: Eq s => [s] -> Parser s [s]
pString [] = pSucceed []
pString (x:xs) = (:) <$> pSym x <*> pString xs

-- Parser para términos "/\"
pTerm :: Parser Char UProp
pTerm = chainlA pFactor (And <$ pString "/\\")

-- Parser para proposiciones "\/"
pProp :: Parser Char UProp
pProp = chainlA pTerm (Or <$ pString "\\/")

-- Aux para parsear los operadores binarios
chainlA :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainlA p op = foldl (flip id) <$> p <*> pList (flip <$> op <*> p)




tests :: IO Counts
tests = do runTestTT allTests

allTests = test [
    "testNumbersAreParsedInVal" ~: testNumbersAreParsedInVal,
    "testNotSymbolParsed" ~: testNotSymbolParsed,
    "testNumberBetweenParensIsParsed" ~: testNumberBetweenParensIsParsed,
    "test3Lt4IsParsed" ~: test3Lt4IsParsed,
    "test3Eq4IsParsed" ~: test3Eq4IsParsed,
    "testPFactorIsParsed" ~: testPFactorIsParsed,
    "testPAndIsParsed" ~: testPAndIsParsed,
    "testPOrIsParsed" ~: testPOrIsParsed
    ]

testNumbersAreParsedInVal = test [
    pVal "3" ~=? [(Val 3, "")],
    pVal "32" ~=? [(Val 32, "")],
    pVal "325" ~=? [(Val 325, "")],
    pVal "325a" ~=? [(Val 325, "a")]
    ]

testNotSymbolParsed = test [
    pNot "~3" ~=? [(Not (Val 3), "")]
    ]

testNumberBetweenParensIsParsed = test [
    pParens "(3)" ~=? [(Val 3, "")],
    pParens "((3))" ~=? [(Val 3, "")]
    ]

test3Lt4IsParsed = test [
    pLt "(3<4)" ~=? [(Lt (Val 3) (Val 4), "")],
    pLt "3<4" ~=? [],
    pLt "3 < 4" ~=? []
    ]

test3Eq4IsParsed = test [
    pEq "(3=4)" ~=? [(Eq (Val 3) (Val 4), "")],
    pEq "3=4" ~=? [],
    pEq "3 = 4" ~=? []
    ]

testPFactorIsParsed = test [
    pFactor "(3<4)" ~=? [(Lt (Val 3) (Val 4),"")],
    pFactor "(3=4)" ~=? [(Eq (Val 3) (Val 4),"")],
    pFactor "(34)" ~=? [(Val 34,"")],
    pFactor "~34" ~=? [(Not (Val 34),"")],
    pFactor "34" ~=? [(Val 34,"")]
    ]

testPAndIsParsed = test [
    pTerm "3" ~=? [(Val 3, "")],
    pTerm "3/\\4" ~=? [(And (Val 3) (Val 4),"")],
    pTerm "~(3/\\4)" ~=? [(Not (And (Val 3) (Val 4)),"")],
    pTerm "(3<4)/\\(3=4)" ~=? [(And (Lt (Val 3) (Val 4)) (Eq (Val 3) (Val 4)),"")]
    ]

testPOrIsParsed = test [
    pProp "3/\\4" ~=? [(And (Val 3) (Val 4),"")],
    pProp "3\\/4" ~=? [(Or (Val 3) (Val 4),"")],
    pProp "~(3\\/4)" ~=? [(Not (Or (Val 3) (Val 4)),"")],
    pProp "(3<4)\\/(3=4)" ~=? [(Or (Lt (Val 3) (Val 4)) (Eq (Val 3) (Val 4)),"")],
    pProp "3" ~=? [(Val 3, "")],
    pProp "~3" ~=? [(Not (Val 3), "")],
    pProp "(3)" ~=? [(Val 3, "")],
    pProp "(3<4)" ~=? [(Lt (Val 3) (Val 4), "")],
    pProp "(3=4)" ~=? [(Eq (Val 3) (Val 4), "")],
    pProp "~(((5=5)\\/~(2<1))/\\((3<4)\\/(7=7)))" ~=? [(Not (And (Or (Eq (Val 5) (Val 5)) (Not (Lt (Val 2) (Val 1)))) (Or (Lt (Val 3) (Val 4)) (Eq (Val 7) (Val 7)))),"")],
    pProp "(~(4<3))\\/(4=3)" ~=? [(Or (Not (Lt (Val 4) (Val 3))) (Eq (Val 4) (Val 3)),"")],
    pProp "(~(4<3))\\/(4=3)asdf" ~=? [(Or (Not (Lt (Val 4) (Val 3))) (Eq (Val 4) (Val 3)),"asdf")]
    ]