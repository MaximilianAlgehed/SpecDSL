{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DeriveFunctor, FlexibleContexts #-}
module Run where
import Control.Monad.Writer.Lazy
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

data Interaction t = Got t | Sent t deriving (Show, Functor)
type Log t = [Interaction (Protocol t)]

-- Checks that a Channel implements the protocol given by
-- a session type by testing it against a random trace that conforms
-- to the type, and produces a log of the interaction.
checkProtocolCompliance ::
    (Implements r t, Checks t r, Channel ch r) =>
    SessionType t   ->
    ch (Protocol r) -> 
    WriterT (Log r) IO Bool
checkProtocolCompliance st ch = lift (generate (implementTrace st)) >>= runHelper
    where
        runHelper Terminate          = return True
        runHelper (Send t (Just tr)) = lift (put ch (Pure t)) >> tell [(Sent (Pure t))] >> runHelper tr
        runHelper (Recv t (Just tr)) = do
                                        x <- lift $ get ch
                                        tell [(Got x)]
                                        case x of
                                            Pure r -> if check t r then
                                                        runHelper tr
                                                      else
                                                        return False
                                            _      -> return False
        runHelper (Branch (Just (l, r))) = do
                                        x <- lift $ get ch
                                        tell [(Got x)]
                                        case x of
                                            ChooseLeft  -> runHelper l
                                            ChooseRight -> runHelper r
                                            _           -> return False
        runHelper (Choose L (Just tr)) = lift (put ch ChooseLeft) >> tell [(Sent ChooseLeft)] >> runHelper tr
        runHelper (Choose R (Just tr)) = lift (put ch ChooseRight) >> tell [(Sent ChooseRight)] >> runHelper tr
        runHelper _                    = return False
