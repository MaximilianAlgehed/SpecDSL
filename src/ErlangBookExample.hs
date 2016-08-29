{-# LANGUAGE MultiParamTypeClasses #-}
module ErlangBookExample where

import Erlang
import Foreign.Erlang
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.Chan
import Typeclasses
import Model
import Test.QuickCheck
import SessionTypes hiding (B)
import JSONType
import LTL
import Run

data Shopping = Book Int | YourBasket [Int] | RequestBasket | Bot deriving (Show)

data ShoppingType = B | YB | RB deriving (Show)

instance Implements Shopping ShoppingType where
    implement B = fmap Book arbitrary
    implement YB = fmap YourBasket arbitrary
    implement RB = return RequestBasket

instance Checks ShoppingType Shopping where
    check B (Book _) = True
    check YB (YourBasket _) = True
    check RB RequestBasket = True
    check _ _ = False

instance Erlang Shopping where
    toErlang (Book b)        = ErlTuple [ErlAtom "book", ErlInt b]
    toErlang (YourBasket xs) = ErlTuple [ErlAtom "yourBasket", ErlList (map ErlInt xs)]
    toErlang RequestBasket   = ErlAtom "requestBasket"

    fromErlang (ErlTuple [ErlAtom "book", ErlInt b]) = Book b
    fromErlang (ErlTuple [ErlAtom "yourBasket", ErlList xs])
        | length [x | ErlInt x <- xs] == length xs = YourBasket [x | ErlInt x <- xs]
        | otherwise = Bot                
    fromErlang (ErlTuple [ErlAtom "yourBasket", ErlNull]) = YourBasket []
    fromErlang (ErlAtom "requestBasket") = RequestBasket
    fromErlang _ = Bot 

-- | The session type for an interaction in our magical book store
bookShopClient :: SessionType ShoppingType
bookShopClient = (!)B :. (bookShopClient :| ((!)RB :. (?)YB)) :. (bookShopClient :| end)

-- | A predicate that says that IF I add a book to the shopping basket,
-- I will see that book in the basket when I request to see my basket
booksPredicate :: LTL (Interaction (Protocol Shopping))
booksPredicate = G $ a (\message -> case message of
                                        Sent (Pure (Book b)) -> U (Not (a isBasket)) (a (contains b))
                                        _                    -> Top
                       )
               where
                isBasket (Got (Pure (YourBasket _))) = Top
                isBasket _                           = Bottom

                contains b (Got (Pure (YourBasket bs)))
                    | b `elem` bs       = Top
                    | otherwise         = Bottom
                contains _ _            = Bottom

testSimpleErlang self = runErlang self "erlangBookExample" "main" bookShopClient booksPredicate
