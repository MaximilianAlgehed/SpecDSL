module Examples where

import Test.QuickCheck
import SessionTypes
import JSONType

shopping :: SessionType JSONType
shopping = addBook :. (shopping :| checkOut)
    where
        addBook = (!) JNumber
        checkOut = (!) JNumber :. (!) JNumber :. (?) JNumber :. end

sizedShopping :: Int -> SessionType JSONType
sizedShopping 0 = (!) JNumber :. end
sizedShopping n = (!) JNumber :. (sizedShopping (n-1) :| (!) JNumber) :. end
