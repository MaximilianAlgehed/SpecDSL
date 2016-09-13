{-# LANGUAGE GADTs, TypeOperators #-}
import qualified Prelude as P
import Test.QuickCheck
import RList

-- | A type for predicates
data Pred a = Pred {image :: RSet a}

-- | A type for functions from a to b
data (a :-> b) = Func {inverse :: RSet b -> RSet a}

-- | An expression for a variable
data (a :~ b) where
    Any   :: (P.Ord a) => (a :~ a)
    Set   :: (P.Ord a) => RSet a -> (a :~ a) 
    Chain :: (P.Ord a, P.Ord b, P.Ord c) => (b :-> c) -> (a :~ b) -> (a :~ c)

($$) :: (P.Ord a, P.Ord b, P.Ord c) => (b :-> c) -> (a :~ b) -> (a :~ c)
($$) = Chain

($!) :: (P.Ord a, P.Ord b) => Pred b -> (a :~ b) -> RSet a
p $! Any         = image p
p $! (Set r)     = intersection r (image p)
p $! (Chain f v) = Pred (inverse f (image p)) $! v 

(...) :: (P.Ord a, P.Enum a) => a -> a -> RSet a
l...h = singletonRange (l, h)

times :: (P.Fractional a, P.Ord a, P.Enum a) => a -> a :-> a
times x = Func P.$ mapMonotonic (P./x)

divide :: (P.Fractional a, P.Ord a, P.Enum a) => a -> a :-> a
divide x = Func P.$ mapMonotonic (P.*x)
