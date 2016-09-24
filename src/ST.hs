{-# LANGUAGE GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts #-}
import Test.QuickCheck
import Data.List
import Foreign.Erlang
import Model
import System.IO
import Control.Concurrent
import Control.Monad.Writer.Lazy
import Control.Concurrent.Chan

data Choice = L | R

instance Arbitrary Choice where
    arbitrary = do
                x <- arbitrary
                if x then
                    return L
                else
                    return R

class a :<: b where
    embed   :: a -> b
    extract :: b -> a

instance a :<: a where
    embed   = id
    extract = id

instance (Erlang a) => a :<: ErlType where
    embed = toErlang
    extract = fromErlang

data ST c where
    Bang   :: (a :<: c) => Gen a -> (a -> Bool) -> (a -> ST c) -> ST c
    Que    :: (a :<: c) => Gen a -> (a -> Bool) -> (a -> ST c) -> ST c 
    Choose :: Gen Choice -> ST c -> ST c -> ST c
    Branch :: Gen Choice -> (Choice -> ST c) -> ST c
    End    :: ST c

dual :: ST a -> ST a
dual (Bang gen pred cont) = Que gen pred (dual . cont)
dual (Que gen pred cont)  = Bang gen pred (dual . cont)
dual (Choose gen l r)     = Branch gen (\c -> case c of
                                            L -> l
                                            R -> r
                                       )
dual (Branch gen cont)    = Choose gen (cont L) (cont R)
dual End                  = End

sessionTest :: (BiChannel ch c)
            => ST c
            -> ch (Protocol c)
            -> WriterT (Log c) IO Bool 
sessionTest (Bang gen _ cont) ch =
    do
        value <- lift $ generate gen
        lift $ put ch $ Pure (embed value)
        tell [Sent (Pure (embed value))]
        sessionTest (cont value) ch
sessionTest (Que _ pred cont) ch =
    do
        Pure value <- lift $ get ch
        tell [Got (Pure value)]
        if pred (extract value) then
            return False
        else
            sessionTest (cont (extract value)) ch
sessionTest (Choose gen l r) ch =
    do
        choice <- lift $ generate gen
        case choice of
            L -> do
                    tell [Sent ChooseLeft]
                    lift $ put ch ChooseLeft
                    sessionTest l ch
            R -> do
                    tell [Sent ChooseRight]
                    lift $ put ch ChooseRight
                    sessionTest r ch
sessionTest (Branch _ cont) ch =
    do
        choice <- lift $ get ch
        tell [Got choice]
        case choice of
            ChooseLeft -> do
                            sessionTest (cont L) ch
            ChooseRight -> do
                            sessionTest (cont R) ch
sessionTest End _ = return True

-- | So that we can talk to Erlang!
instance (Erlang t) => Erlang (Protocol t) where
    toErlang (Pure t)    = ErlTuple [ErlAtom "pure", toErlang t]
    toErlang ChooseLeft  = ErlAtom "chooseLeft"
    toErlang ChooseRight = ErlAtom "chooseRight"

    fromErlang (ErlTuple [ErlAtom "pure", t]) = Pure (fromErlang t)
    fromErlang (ErlAtom "chooseLeft")         = ChooseLeft
    fromErlang (ErlAtom "chooseRight")        = ChooseRight

runErlang :: (Erlang t, Show t)
          => Self -- Created by "createSelf \"name@localhost\""
          -> String -- module name
          -> String -- function name
          -> ST t -- The session type for the interaction
          -> IO ()
runErlang self mod fun st = quickTest (runfun self) st
    where
        runfun :: (Erlang r) => Self -> P Chan (Protocol r) -> IO ()
        runfun self ch =
            do
                mbox <- createMBox self
                rpcCall mbox (Short "erl") mod fun []
                id1 <- forkIO $ erlangLoop ch mbox
                id2 <- forkIO $ haskellLoop ch mbox
                return ()

        finish pid id1 id2 =
            do
                killThread id1
                killThread id2

        erlangLoop ch mbox =
            do
               m <- mboxRecv mbox
               put ch $ fromErlang m
               erlangLoop ch mbox
        
        haskellLoop ch mbox =
            do
                m <- get ch
                mboxSend mbox (Short "erl") (Right "p") (mboxSelf mbox, m)
                haskellLoop ch mbox

-- | Run multiple tests to find a counterexample
-- | that shows that a process does not implement
-- | a session type OR it does not comply with the
-- | spec in the form of an LTL formula
quickTest :: (BiChannel ch c, Show c)
          => (ch (Protocol c) -> IO ()) -- Function to test
          -> ST c                       -- The session type for the interaction
          -> IO ()
quickTest impl t = loop 100
    where
        loop 0 = putStrLn "\rO.K"
        loop n = do
                    hPutStr stderr $ "\r                  \r"
                    hPutStr stderr $ show n
                    ch <- new
                    forkIO $ impl (bidirect ch)
                    (b, w) <- runWriterT $ sessionTest t ch
                    kill ch
                    if b then
                        loop (n-1)
                    else
                        do
                            hPutStr stderr $ "\r                  \r"
                            putStrLn $ "Failed after "++(show (100 - n))++" tests"
                            putStrLn "With:"
                            putStrLn (show w)
                            return ()

{- A translation of the bookshop example -}
bookShop :: ST ErlType
bookShop = bookShop' ([] :: [Int])

data Request = RequestBook 

instance Erlang Request where
    toErlang _ = ErlAtom "unit"

    fromErlang (ErlAtom "unit") = RequestBook

bookShop' bs = Bang (fmap abs arbitrary) (>0)
                    (\b -> Choose
                            arbitrary
                            (bookShop' (b:bs))
                            (Bang (return RequestBook) (const True)
                                (\i -> Que (return (b:bs)) ((sort bs ==) . sort)
                                        (\bs' -> Choose
                                                    arbitrary
                                                    (bookShop' bs')
                                                    End
                                        )
                                )
                            )
                    )
