{-# LANGUAGE MultiParamTypeClasses #-}
module Generator where
import Test.QuickCheck

class Implements a b where
    implement :: b -> Gen a
