module LTL where

-- LTL defined by a set of adequate connectives
-- This is the minimal definition of LTL
data LTL domain = Atomic (domain -> Bool)
                | Terminated
                | And (LTL domain) (LTL domain)
                | Not (LTL domain)
                | X (LTL domain)
                | U (LTL domain) (LTL domain)
                | G (LTL domain)

-- Check if an execution trace satisfies an LTL formula
-- This needs to be extended to fit finite traces
check :: LTL domain -> [domain] -> Bool
check Terminated []    = True
check (Atomic f) (x:_) = f x
check (And l r) xs     = check l xs && check r xs
check (Not ltl) xs     = not $ check ltl xs
check (X ltl) (_:xs)   = check ltl xs
check (U l r) xs       = case y of
                            -- r eventually becomes true
                            Just y' -> and [check l (drop n xs) | n <- [0..y'-1]]
                            -- r never becomes true
                            Nothing -> False
    where
        y = fir (Just 0) r xs
        fir n f [] = if check f [] then n else Nothing
        fir n f t  = if check f t then n else fir (fmap (+1) n) f (tail t)
check (G ltl) xs       = and [check ltl (drop n xs) | n <- [0..(length xs - 1)]]
check _ _              = False

-- Some nice shorthands
a .| b = Not (And (Not a) (Not b))
(.&) = And
a = Atomic
