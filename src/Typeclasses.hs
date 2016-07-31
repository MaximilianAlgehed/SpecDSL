{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Typeclasses where
import Test.QuickCheck

class Implements a b where
    implement :: b -> Gen a

class Checks a b where
    check :: a -> b -> Bool
