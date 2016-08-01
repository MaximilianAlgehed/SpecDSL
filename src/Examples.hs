module Examples where

import Test.QuickCheck
import SessionTypes
import JSONType
import LTL

shopping :: SessionType JSONType
shopping = addBook :. (shopping :| checkOut)
    where
        addBook = (!) JNumber
        checkOut = (!) JNumber :. (!) JNumber :. (?) JNumber :. end

sizedShopping :: Int -> SessionType JSONType
sizedShopping 0 = (!) JNumber :. end
sizedShopping n = (!) JNumber :. (sizedShopping (n-1) :| (!) JNumber) :. end

-- | Check that once a book is ordered, it will show up in the next
-- | list of books
data Shopping = Book Int | YourBasket [Int] | WhateverNonense deriving (Show)

booksPredicate :: LTL Shopping
booksPredicate = a (\message -> case message of
                        Book b -> U (Not (a isBasket)) (a (contains b))
                        _      -> Top
                   )
               where
                isBasket (YourBasket _) = Top
                isBasket _              = Bottom

                contains b (YourBasket bs)
                    | b `elem` bs       = Top
                    | otherwise         = Bottom
                contains _ _            = Bottom
