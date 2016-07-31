import Test.QuickCheck
import SessionTypes
import JSONType

shopping :: SessionType JSONType
shopping = dual $ addBook :. (shopping :| checkOut)
    where
        addBook = (!) (JObject []) 
        checkOut = (!) (JObject []) :. (!) (JObject []) :. (?) (JObject []) :. end

--testShopping :: IO (Trace (Session 
