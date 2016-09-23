{-# LANGUAGE GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts #-}
import Test.QuickCheck
import Data.List
import Foreign.Erlang

data Choice = L | R

instance Arbitrary Choice where
    arbitrary = do
                x <- arbitrary
                if x then
                    return L
                else
                    return R

class a :<: b where
    embed   :: a -> b
    extract :: b -> a

instance a :<: a where
    embed   = id
    extract = id

instance (Erlang a) => a :<: ErlType where
    embed = toErlang
    extract = fromErlang

data ST c where
    Bang   :: (a :<: c) => Gen a -> (a -> Bool) -> (a -> ST c) -> ST c
    Que    :: (a :<: c) => Gen a -> (a -> Bool) -> (a -> ST c) -> ST c 
    Choose :: Gen Choice -> ST c -> ST c -> ST c
    Branch :: Gen Choice -> (Choice -> ST c) -> ST c
    End    :: ST c

dual :: ST a -> ST a
dual (Bang gen pred cont) = Que gen pred (dual . cont)
dual (Que gen pred cont)  = Bang gen pred (dual . cont)
dual (Choose gen l r)     = Branch gen (\c -> case c of
                                            L -> l
                                            R -> r
                                       )
dual (Branch gen cont)    = Choose gen (cont L) (cont R)
dual End                  = End

{- A translation of the bookshop example in to this view of session types -}
bookShop' bs = Bang arbitrary (const True)
                    (\b -> Choose
                            arbitrary
                            (bookShop' (b:bs))
                            (Bang arbitrary (const True)
                                (\i -> Que (return (b:bs)) (isSubsequenceOf (take i bs))
                                        (\bs' -> Choose
                                                    arbitrary
                                                    (bookShop' bs')
                                                    End
                                        )
                                )
                            )
                    )
