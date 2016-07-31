{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DeriveFunctor, FlexibleContexts #-}
module Run where
import Control.Concurrent.Chan
import Test.QuickCheck
import Typeclasses 
import SessionTypes
import JSONType

data Protocol t = Pure t
                | ChooseLeft
                | ChooseRight
                deriving (Show, Functor)

class Channel ch t where
    new :: IO (ch (Protocol t))
    put :: ch (Protocol t) -> Protocol t -> IO ()
    get :: ch (Protocol t) -> IO (Protocol t)

instance Channel Chan JSONValue where
    new = newChan
    put = writeChan
    get = readChan

checkProtocolCompliance :: (Implements r t, Checks t r, Channel ch r) => SessionType t -> ch (Protocol r) ->  IO Bool
checkProtocolCompliance st ch = generate (implementTrace st) >>= runHelper
    where
        runHelper Terminate          = return True
        runHelper (Send t (Just tr)) = put ch (Pure t) >> runHelper tr
        runHelper (Recv t (Just tr)) = do
                                        x <- get ch
                                        case x of
                                            Pure r -> if check t r then
                                                        runHelper tr
                                                      else
                                                        return False
                                            _      -> return False
        runHelper (Branch (Just (l, r))) = do
                                        x <- get ch
                                        case x of
                                            ChooseLeft  -> runHelper l
                                            ChooseRight -> runHelper r
                                            _           -> return False
        runHelper (Choose L (Just tr)) = put ch ChooseLeft >> runHelper tr
        runHelper (Choose R (Just tr)) = put ch ChooseRight >> runHelper tr
        runHelper _                    = return False
