{-# LANGUAGE MultiParamTypeClasses #-}
module Examples where

import Control.Concurrent
import Control.Concurrent.Chan
import Typeclasses
import Model
import Test.QuickCheck
import SessionTypes hiding (B)
import JSONType
import LTL
import Run

shopping :: SessionType JSONType
shopping = addBook :. (shopping :| checkOut)
    where
        addBook = (!) JNumber
        checkOut = (!) JNumber :. (!) JNumber :. (?) JNumber :. end

sizedShopping :: Int -> SessionType JSONType
sizedShopping 0 = (!) JNumber :. end
sizedShopping n = (!) JNumber :. (sizedShopping (n-1) :| (!) JNumber) :. end


{- What follows is a toy example worked out for the purpose of
 - showing what is possible with the system. The example shows
 - how we can specify the correctness of a shopping system -} 

data Shopping = Book Int | YourBasket [Int] | RequestBasket deriving (Show)

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

-- | The session type for an interaction in our magical book store
bookShopClient :: SessionType ShoppingType
bookShopClient = (!)B :. (bookShopClient :| ((!)RB :. (?)YB)) :. end

-- | A predicate that says that IF I add a book to the shopping basket,
-- I will see that book in the basket when I request to see my basket
booksPredicate :: LTL (Interaction (Protocol Shopping))
booksPredicate = G $ a (\message -> case message of
                        Sent (Pure (Book b)) -> U (Not (a isBasket)) (a (contains b))
                        _            -> Top
                   )
               where
                isBasket (Got (Pure (YourBasket _))) = Top
                isBasket _                           = Bottom

                contains b (Got (Pure (YourBasket bs)))
                    | b `elem` bs       = Top
                    | otherwise         = Bottom
                contains _ _            = Bottom

bookServer :: (Chan (Protocol Shopping), Chan (Protocol Shopping)) -> IO ()
bookServer (chi, cho) = loop []
    where
        loop xs = do
                    (Pure (Book b)) <- readChan chi

                    -- There is a bug RIGHT HERE
                    -- for demonstration purposes!
                    if b > 0 then
                        awaitBranch (b:xs)
                    else
                        awaitBranch xs

        awaitBranch xs = do
                    br <- readChan chi
                    case br of
                        ChooseLeft -> loop xs
                        ChooseRight -> do
                                         Pure RequestBasket <- readChan chi
                                         writeChan cho (Pure (YourBasket xs))
                                         return ()
                        _           -> return ()

testBooks = do
                ch <- newChan
                ch' <- newChan
                forkIO $ bookServer (ch, ch')
                test bookShopClient (P ch' ch) booksPredicate

