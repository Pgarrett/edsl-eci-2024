class Expr e where
    val    :: Int -> e Int
    eq     :: e Int -> e Int -> e Bool
    lt     :: e Int -> e Int -> e Bool
    not    :: e Bool -> e Bool
    and    :: e Bool -> e Bool -> e Bool
    or     :: e Bool -> e Bool -> e Bool

data Eval t = E t

instance Expr Eval where
    val x           = E x
    eq (E x) (E y)  = E (x == y)
    lt (E x) (E y)  = E (x < y)
    not (E b)       = E (Prelude.not b)
    and (E b) (E c) = E (b && c)
    or (E b) (E c)  = E (b || c)


-- case (val 4) of E result -> print result
-- case (eq (val 3) (val 4)) of E result -> print result
-- case (eq (val 4) (val 4)) of E result -> print result
-- case (lt (val 3) (val 4)) of E result -> print result
-- case (lt (val 5) (val 4)) of E result -> print result
-- case (not (lt (val 3) (val 4))) of E result -> print result
-- case (and (lt (val 3) (val 4)) (lt (val 5) (val 0)) ) of E result -> print result
-- case (or (lt (val 3) (val 4)) (lt (val 5) (val 0)) ) of E result -> print result