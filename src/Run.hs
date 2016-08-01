{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, FlexibleContexts, FlexibleInstances #-}
module Run where

import Control.Concurrent
import Control.Monad.Writer.Lazy
import Control.Concurrent.Chan
import Test.QuickCheck
import Typeclasses 
import SessionTypes
import JSONType
import Model
import qualified LTL as LTL

data P a x = P (a x) (a x)

class BiChannel ch t where
    new :: IO (ch (Protocol t))
    put :: ch (Protocol t) -> Protocol t -> IO ()
    get :: ch (Protocol t) -> IO (Protocol t)
    bidirect :: ch (Protocol t) -> ch (Protocol t)

instance BiChannel (P Chan) a where
    new = do
            cin <- newChan
            cout <- newChan
            return $ P cin cout
    put (P cin cout) = writeChan cout
    get (P cin cout) = readChan cin
    bidirect (P cin cout) = P cout cin

-- Checks that a BiChannel implements the protocol given by
-- a session type by testing it against a random trace that conforms
-- to the type, and produces a log of the interaction.
checkProtocolCompliance ::
    (Implements r t, Checks t r, BiChannel ch r) =>
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

-- | Test if a channel implements a session type
-- | and a predicate in LTL holds for the interaction
test :: (Implements r t, Checks t r, BiChannel ch r, Show r) =>
    SessionType t ->
    ch (Protocol r) ->
    LTL.LTL (Interaction (Protocol r)) ->
    IO Bool
test t ch pred = do
                    (b, logs) <- runWriterT (checkProtocolCompliance t ch)
                    if not b then
                        do
                            putStrLn $ "Process does not implement session type"
                            putStrLn $ show logs
                            return False
                    else
                       if not (LTL.check pred logs) then
                            do
                                putStrLn $ "Predicate counterexample"
                                putStrLn $ show logs
                                return False
                       else
                            return True

quickTest :: (Implements r t, Checks t r, BiChannel ch r, Show r) =>
    (ch (Protocol r) -> IO ()) ->
    SessionType t ->
    LTL.LTL (Interaction (Protocol r)) ->
    IO ()
quickTest impl t pred = loop 100
    where
        loop 0 = putStrLn "O.K"
        loop n = do
                    putStrLn $ show n
                    ch <- new
                    forkIO $ impl (bidirect ch)
                    b <- test t ch pred
                    if b then
                        loop (n-1)
                    else
                        return ()
