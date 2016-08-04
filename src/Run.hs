{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, FlexibleContexts, FlexibleInstances #-}
module Run where

import System.IO
import Control.Concurrent
import Control.Monad.Writer.Lazy
import Control.Concurrent.Chan
import Test.QuickCheck
import Typeclasses 
import SessionTypes
import JSONType
import Model
import qualified LTL as LTL

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
                    kill ch
                    if not b then
                        do
                            hPutStr stderr "\r                               \r"
                            putStrLn $ "Process does not implement session type"
                            putStrLn $ show logs
                            return False
                    else
                       if not (LTL.check pred logs) then
                            do
                                hPutStr stderr "\r                               \r"
                                putStrLn $ "Predicate counterexample"
                                putStrLn $ show logs
                                return False
                       else
                            return True

-- | Run multiple tests to find a counterexample
-- | that shows that a process does not implement
-- | a session type OR it does not comply with the
-- | spec in the form of an LTL formula
quickTest :: (Implements r t, Checks t r, BiChannel ch r, Show r) =>
    (ch (Protocol r) -> IO ()) ->         -- Function to test
    SessionType t ->                      -- The session type for the interaction
    LTL.LTL (Interaction (Protocol r)) -> -- The LTL predicate
    IO ()
quickTest impl t pred = loop 1000
    where
        loop 0 = putStrLn "\rO.K"
        loop n = do
                    hPutStr stderr $ "\r                  \r"
                    hPutStr stderr $ show n
                    ch <- new
                    forkIO $ impl (bidirect ch)
                    b <- test t ch pred
                    kill ch
                    if b then
                        loop (n-1)
                    else
                        do
                            putStrLn $ "After "++(show (1000 - n))++" tests"
                            return ()
