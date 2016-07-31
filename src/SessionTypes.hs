{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module SessionTypes where
import Test.QuickCheck
import Generator

data SessionType t = B t -- Send something
                   | Q t -- Get something
                   | (SessionType t) :& (SessionType t) -- Branch
                   | (SessionType t) :| (SessionType t) -- Choice
                   | (SessionType t) :. (SessionType t) -- Composition
                   | End -- Termination
                   deriving (Show, Eq, Functor)

data Choice = L | R deriving (Show)

data Trace t = Send t (Maybe (Trace t))
             | Recv (Maybe (Trace t))
             | Branch (Maybe (Trace t, Trace t))
             | Choose Choice (Maybe (Trace t))
             | Terminate
             deriving (Show, Functor)

appendTrace :: Trace t -> Trace t -> Trace t
appendTrace Terminate _                = Terminate
appendTrace (Send t Nothing) tr        = Send t $ Just tr
appendTrace (Send t (Just tr)) tr'     = Send t $ Just $ tr `appendTrace` tr'
appendTrace (Branch Nothing) tr        = Branch $ Just (tr, tr)
appendTrace (Branch (Just (t, t'))) tr = Branch $ Just (t `appendTrace` tr, t' `appendTrace` tr)
appendTrace (Choose c Nothing) tr      = Choose c $ Just tr
appendTrace (Choose c (Just t)) tr     = Choose c $ Just $ t `appendTrace` tr
appendTrace (Recv Nothing) tr          = Recv $ Just tr
appendTrace (Recv (Just t)) tr         = Recv $ Just $ t `appendTrace` tr

-- | This function is buggy at the moment, it does not work branches
implementTrace :: (Implements t t') => SessionType t' -> Gen (Trace t) 
implementTrace (B t)       = do
                                t' <- implement t
                                return $ Send t' Nothing 
implementTrace (Q t)       = return $ Recv $ Nothing
implementTrace (st :| st') = oneof $ [fmap ((Choose L) . Just) (implementTrace st), fmap ((Choose R) . Just) (implementTrace st')]
implementTrace (st :& st') = do
                                t  <- implementTrace st
                                t' <- implementTrace st'
                                return $ Branch $ Just (t, t')
implementTrace (st :. st') = do
                                t  <- implementTrace st
                                t' <- implementTrace st'
                                return $ t `appendTrace` t'
implementTrace End         = return Terminate

instance (Implements t t') => Implements (Trace t) (SessionType t') where
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
