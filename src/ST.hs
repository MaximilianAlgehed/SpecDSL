{-# LANGUAGE GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts #-}
import Test.QuickCheck
import Data.List
import Foreign.Erlang
import BiCh 
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
    extract :: b -> Maybe a

instance a :<: a where
    embed   = id
    extract = Just . id

type Predicate a = (Gen a, a -> Bool)

instance (Erlang a) => a :<: ErlType where
    embed = toErlang
    extract = Just . fromErlang

data ST c where
    Bang   :: (a :<: c) => Predicate a -> (a -> ST c) -> ST c
    Que    :: (a :<: c) => Predicate a -> (a -> ST c) -> ST c 
    Choose :: Gen Choice -> ST c -> ST c -> ST c
    Branch :: Gen Choice -> ST c -> ST c -> ST c
    End    :: ST c

dual :: ST a -> ST a
dual (Bang pred cont) = Que pred (dual . cont)
dual (Que pred cont)  = Bang pred (dual . cont)
dual (Choose gen l r) = Branch gen l r 
dual (Branch gen l r) = Choose gen l r
dual End              = End

sessionTest :: (BiChannel ch c)
            => ST c
            -> ch (Protocol c)
            -> WriterT (Log c) IO Bool 
sessionTest (Bang (gen, _) cont) ch =
    do
        value <- lift $ generate gen
        lift $ put ch $ Pure (embed value)
        tell [Sent (Pure (embed value))]
        sessionTest (cont value) ch
sessionTest (Que (_, pred) cont) ch =
    do
        Pure mv <- lift $ get ch
        tell [Got (Pure mv)]
        case extract mv of
            Just value -> if pred value then
                            sessionTest (cont value) ch
                          else
                            return False
            Nothing    -> return False
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
sessionTest (Branch _ l r) ch =
    do
        choice <- lift $ get ch
        tell [Got choice]
        case choice of
            ChooseLeft -> do
                            sessionTest l ch
            ChooseRight -> do
                            sessionTest r ch
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

-- Run some tests
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

-- Some generator-predicate pairs
posNum :: (Ord a, Num a, Arbitrary a) => Predicate a
posNum = (fmap ((+1) . abs) arbitrary, (>0))

is :: (Eq a) => a -> Predicate a
is a = (return a, (a==))

isPermutation :: (Ord a) => [a] -> Predicate [a]
isPermutation bs = (shuffle bs, ((sort bs) ==) . sort)

(|||) :: Predicate a -> Predicate a -> Predicate a
(lg, l) ||| (rg, r) = (oneof [lg, rg], (\a -> l a || r a))

(&&&) :: Predicate a -> Predicate a -> Predicate a
(lg, l) &&& (rg, r) = (oneof [lg, rg] `suchThat` p, p)
    where
        p a = r a && l a

any :: (Arbitrary a) => Predicate a
any = (arbitrary, const True)

(<|>) = Choose arbitrary
infixr 0 <|>

(<&>) = Branch arbitrary
infixr 0 <&>

f .- c = f (const c)
infixr 0 .-

{- An example of "buying books from amazon" -}
bookShop :: ST ErlType
bookShop = bookShop' ([] :: [Int])

data Request = RequestBooks deriving Eq

instance Erlang Request where
    toErlang _ = ErlAtom "requestBooks"

    fromErlang (ErlAtom "requestBooks") = RequestBooks

-- This demonstrates that syntax is an issue :/
-- maybe Koen has some nice ideas for combinators
-- that could make this very pretty indeed
bookShop' bs = Bang posNum $
               \b -> let bs' = b:bs in
                   (bookShop' bs')
                   <|>
                   Bang (is RequestBooks) $
                        \i -> Que (isPermutation bs') $
                        \bs' ->
                            (bookShop' bs')
                            <|>
                            End

{- The example from POPL SRC -}
client :: ST ErlType
client =
    Bang validPlate .-
    Que (is "ERR_CAR_NOT_FOUND" ||| validTaxID) .-
    client <|> End

-- Dummy predicates
validPlate :: Predicate String
validPlate = (arbitrary, const True)

validTaxID :: Predicate String
validTaxID = (arbitrary, const True)
