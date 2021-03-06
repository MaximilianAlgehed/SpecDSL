{-# LANGUAGE GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts #-}
import Prelude hiding (any)
import Test.QuickCheck
import Data.List hiding (any)
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

type Predicate a = (Gen a, a -> Maybe String)

predicate :: String -> (Gen a, (a -> Bool)) -> Predicate a
predicate s (g, p) = (g, \a -> guard (not (p a)) >> return s)

instance (Erlang a) => a :<: ErlType where
    embed = toErlang
    extract = Just . fromErlang

data ST c where
    Send   :: (Show a, a :<: c) => Predicate a -> (a -> ST c) -> ST c
    Get    :: (Show a, a :<: c) => Predicate a -> (a -> ST c) -> ST c 
    Choose :: Gen Choice -> ST c -> ST c -> ST c
    Branch :: Gen Choice -> ST c -> ST c -> ST c
    End    :: ST c

dual :: ST a -> ST a
dual (Send pred cont) = Get pred (dual . cont)
dual (Get pred cont)  = Send pred (dual . cont)
dual (Choose gen l r) = Branch gen l r 
dual (Branch gen l r) = Choose gen l r
dual End              = End

sessionTest :: (BiChannel ch c)
            => ST c
            -> ch (Protocol c)
            -> WriterT (Log c) IO (Maybe String)
sessionTest (Send (gen, _) cont) ch =
    do
        value <- lift $ generate gen
        lift $ put ch $ Pure (embed value)
        tell [Sent (Pure (embed value))]
        sessionTest (cont value) ch
sessionTest (Get (_, pred) cont) ch =
    do
        Pure mv <- lift $ get ch
        tell [Got (Pure mv)]
        case extract mv of
            Just value -> if pred value == Nothing then
                            sessionTest (cont value) ch
                          else
                            return $ fmap (++" "++(show value)) (pred value)
            Nothing    -> return $ Just "Type error!"
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
sessionTest End _ = return Nothing

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
runErlang self mod fun st = specCheck (runfun self) st
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
specCheck :: (BiChannel ch c, Show c)
          => (ch (Protocol c) -> IO ()) -- Function to test
          -> ST c                       -- The session type for the interaction
          -> IO ()
specCheck impl t = loop 100
    where
        loop 0 = putStrLn "\rO.K"
        loop n = do
                    hPutStr stderr $ "\r                  \r"
                    hPutStr stderr $ show n
                    ch <- new
                    forkIO $ impl (bidirect ch)
                    (b, w) <- runWriterT $ sessionTest t ch
                    kill ch
                    if b == Nothing then
                        loop (n-1)
                    else
                        do
                            hPutStr stderr $ "\r                  \r"
                            putStrLn $ "Failed after "++(show (100 - n))++" tests"
                            putStrLn $ "With: "++(fromJust b)
                            putStrLn "In:"
                            putStrLn "---"
                            sequence_ $ map (putStrLn . ("    "++) . printTrace) w
                            putStrLn "---"
                            return ()

printTrace (Got (Pure x))   = "Got ("++(show x)++")"
printTrace (Got ChooseLeft) = "Branched left"
printTrace (Got ChooseRight) = "Branched right"
printTrace (Sent (Pure x))  = "Sent ("++(show x)++")"
printTrace (Sent ChooseLeft) = "Chose left"
printTrace (Sent ChooseRight) = "Chose right"

fromJust (Just x) = x

-- Some generator-predicate pairs
posNum :: (Ord a, Num a, Arbitrary a) => Predicate a
posNum = predicate "posNum" (fmap ((\x -> x-1) . abs) arbitrary, (>0))

is :: (Eq a, Show a) => a -> Predicate a
is a = predicate ("is "++(show a)) (return a,(a==))

isPermutation :: (Ord a, Show a) => [a] -> Predicate [a]
isPermutation bs = predicate ("isPermutation " ++ (show bs)) (shuffle bs, (((sort bs) ==) . sort))

(|||) :: Predicate a -> Predicate a -> Predicate a
(lg, l) ||| (rg, r) = (oneof [lg, rg], disj l r)
    where
        disj l r a = do
                        sl <- l a
                        sr <- r a
                        return $ "("++ sl ++ " || " ++ sr ++ ")"

(&&&) :: Predicate a -> Predicate a -> Predicate a
(lg, l) &&& (rg, r) = (oneof [lg, rg] `suchThat` (\a -> p a == Nothing), p)
    where
        p a = case r a of
                Nothing -> case l a of
                            Nothing -> Nothing
                            Just s  -> Just s
                Just s  -> Just s

any :: (Arbitrary a) => Predicate a
any = (arbitrary, const Nothing)

(<|>) = Choose arbitrary
infixr 0 <|>

(<&>) = Branch arbitrary
infixr 0 <&>

f .- c = f (const c)
infixr 0 .-

{- An example of "buying books from amazon" -}
bookShop :: ST ErlType
bookShop = bookShop' ([] :: [Int])

data Request = RequestBooks deriving (Eq, Show)

instance Erlang Request where
    toErlang _ = ErlAtom "requestBooks"

    fromErlang (ErlAtom "requestBooks") = RequestBooks

-- This demonstrates that syntax is an issue :/
-- maybe Koen has some nice ideas for combinators
-- that could make this very pretty indeed
bookShop' bs = Send posNum $
               \b -> let bs' = b:bs in
                   (bookShop' bs')
                   <|>
                   Send (is RequestBooks) $
                        \i   -> Get (isPermutation bs') $
                        \bs' -> (bookShop' bs') <|> End

{- The new example from POPL SRC -}
protocol :: ([Action] :<: t, [Status] :<: t, [Double] :<: t) => ST t
protocol = Send validData .- (execActs <&> continue)

execActs :: ([Action] :<: t, [Status] :<: t, [Double] :<: t) => ST t
execActs =
    Get validActs $ \acts ->
    Send (validStatus acts) .-
    continue

continue :: ([Action] :<: t, [Status] :<: t, [Double] :<: t) => ST t
continue = protocol <|> End

validData = predicate "validData" (sequence $ [arbitrary, arbitrary, arbitrary], \xs -> length (xs :: [Double]) == 3 )

data Action = Output Int Int | Input Int

instance Show Action where
    show (Output i j) = "Output "++(show i)++" high for "++(show j)++" seconds"
    show (Input i)    = "Get input "++(show i)

instance Arbitrary Action where
    arbitrary = oneof [do
                        x <- arbitrary
                        y <- arbitrary
                        return $ Output x y,
                       fmap Input arbitrary]

data Status = Out Int Bool | Inp Int Double

instance Show Status where
    show (Out i b) = "Output "++(show i)++" "++(if b then "OK" else "ERR")
    show (Inp i d) = "Input "++(show i)++" "++(show d)

validActs = any

validStatus acts = predicate "validStatus" (sequence (map mkGen acts), \xs -> and $ zipWith isValid acts xs)
    where
        isValid (Output _ _) (Inp _ _) = False
        isValid (Output i _) (Out j _) = i == j
        isValid (Input i) (Out _ _) = False
        isValid (Input i) (Inp j _)    = i == j
        
        mkGen (Output i _) = fmap (Out i) arbitrary
        mkGen (Input i)    = fmap (Inp i) arbitrary
