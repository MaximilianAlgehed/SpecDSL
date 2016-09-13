{-# LANGUAGE TemplateHaskell #-}
import qualified Prelude as P
import Language.Haskell.TH

data Nat = Z | S Nat deriving P.Show

data Bool = True | False deriving P.Show

eq :: Nat -> Nat -> Bool
eq Z Z         = True
eq (S l) (S r) = eq l r
eq _     _     = False

{-
 - eqII True (S (S (S Z))) = S <$> eqII (S (S Z))
 -                         = S <$> S <$> eqII (S Z)
 -                         = S <$> S <$> S <$> eqII Z 
 -                         = S <$> S <$> S <$> return Z
 - We get the singleton set
 - { (S (S (S Z))) }
 -}

lt :: Nat -> Nat -> Bool
lt Z _         = False
lt (S _) Z     = True
lt (S l) (S r) = lt l r

{-
 - ltII True (S (S (S Z))) = Z <|> S <$> ltII (S (S Z))
 -                         = Z <|> S <$> Z <|> S <$> ltII (S Z)
 -                         = Z <|> S <$> Z <|> S <$> Z <|> ltII Z
 -                         = Z <|> S <$> Z <|> S <$> Z <|> Empty
 - We get the set
 - { Z, (S Z), (S (S Z)) }
 -}

gt :: Nat -> Nat -> Bool
gt Z Z         = False
gt Z (S _)     = True
gt (S l) (S r) = gt l r

{- 
 - gtII True (S (S (S Z))) = S <$> gtII (S (S Z))
 -                         = S <$> S <$> gtII (S Z)
 -                         = S <$> S <$> S <$> gtII Z
 -                         = S <$> S <$> S <$> S <$> Any
 - We get the, lazy, set
 - { S (S (S (S Z))), S (S (S (S (S Z)))), ... }
 -}

data List a = N | C a (List a)

length :: List a -> Nat
length N        = Z
length (C _ xs) = S (length xs)

{- 
 - lengthII Z = N
 -
 - lengthII (S (S Z)) = C Any <$> lengthII (S Z)
 -                    = C Any <$> C Any <$> lengthII Z
 -                    = C Any <$> C Any <$> N
 -
 - With FEAT this set should also
 - make for a lazy enumeration
 - { (C Z (C Z N)), (C Z (C (S Z) N)), (C (S Z) (C Z N)), (C Z (C (S (S Z)) N)), ... }
 -}

short xs = lt (S (S (S Z))) (length xs)

{-
 - shortII True = lengthII (ltII True (S (S (S Z))))
 -              = map lengthII { Z, (S Z), (S (S Z)) }
 -              = concat (balance { lengthII Z, lengthII (S Z), lengthII (S (S Z)) })
 -              = concat { { N }, ... }
 -}
