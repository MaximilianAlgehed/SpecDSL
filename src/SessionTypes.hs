{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module SessionTypes where
import Test.QuickCheck
import Typeclasses

-- | Session type parameterized over some
-- | type universe t
data SessionType t = B t -- Send something
                   | Q t -- Get something
                   | (SessionType t) :& (SessionType t) -- Branch
                   | (SessionType t) :| (SessionType t) -- Choice
                   | (SessionType t) :. (SessionType t) -- Composition
                   | End -- Termination
                   deriving (Show, Eq, Functor)

-- | Go left or go right
data Choice = L | R deriving (Show)

data Trace t t' = Send t (Maybe (Trace t t'))
                | Recv t' (Maybe (Trace t t'))
                | Branch (Trace t t', Trace t t')
                | Choose Choice (Trace t t')
                | Terminate
                deriving (Show, Functor)

appendTrace :: Trace t t' -> Trace t t' -> Trace t t'
appendTrace Terminate _            = Terminate
appendTrace (Send t Nothing) tr    = Send t $ Just tr
appendTrace (Send t (Just tr)) tr' = Send t $ Just $ tr `appendTrace` tr'
appendTrace (Branch (t, t')) tr    = Branch $ (t `appendTrace` tr, t' `appendTrace` tr)
appendTrace (Choose c t) tr        = Choose c $ t `appendTrace` tr
appendTrace (Recv x Nothing) tr    = Recv x $ Just tr
appendTrace (Recv x (Just t)) tr   = Recv x $ Just $ t `appendTrace` tr

implementTrace :: (Implements t t') => SessionType t' -> Gen (Trace t t') 
implementTrace (B t)       = do
                                t' <- implement t
                                return $ Send t' Nothing 
implementTrace (Q t)       = return $ Recv t $ Nothing
implementTrace (st :| st') = oneof $ [fmap (Choose L) (implementTrace st), fmap (Choose R) (implementTrace st')]
implementTrace (st :& st') = do
                                t  <- implementTrace st
                                t' <- implementTrace st'
                                return $ Branch (t, t')
implementTrace (st :. st') = do
                                t  <- implementTrace st
                                t' <- implementTrace st'
                                return $ t `appendTrace` t'
implementTrace End         = return Terminate

instance (Implements t t') => Implements (Trace t t') (SessionType t') where
    implement = implementTrace

dual :: SessionType t -> SessionType t
dual (B t)     = Q t
dual (Q t)     = B t
dual (t :& t') = (dual t) :| (dual t')
dual (t :| t') = (dual t) :& (dual t')
dual (t :. t') = (dual t) :. (dual t')
dual t         = t

end :: SessionType t
end = End

(!) :: t -> SessionType t
(!) = B

(?) :: t -> SessionType t
(?) = Q
