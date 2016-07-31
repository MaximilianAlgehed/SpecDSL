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

data Session typeReal typeTemplate = P typeReal
                                   | L (Session typeReal typeTemplate)
                                   | R (Session typeReal typeTemplate)
                                   deriving (Show)

data Trace t = Send t (Maybe (Trace t))
             | Recv (Either (Maybe (Trace t)) (Trace t, Trace t))
             | Terminate
             deriving (Show)

instance Functor Trace where
    fmap f (Send t m) = Send (f t) (fmap (fmap f) m)
    fmap f (Recv (Left m)) = Recv (Left (fmap (fmap f) m))
    fmap f (Recv (Right (t, t'))) = Recv (Right (fmap f t, fmap f t'))
    fmap _ Terminate = Terminate

appendTrace :: Trace t -> Trace t -> Trace t
appendTrace Terminate _               = Terminate
appendTrace (Send t Nothing) tr       = Send t $ Just tr
appendTrace (Send t (Just tr)) tr'    = Send t $ Just $ tr `appendTrace` tr'
appendTrace (Recv (Left (Just t))) tr = Recv $ Left $ Just $ t `appendTrace` tr
appendTrace (Recv (Left Nothing)) tr  = Recv $ Left $ Just tr
appendTrace (Recv (Right (t, t'))) tr = Recv $ Right (t `appendTrace` tr, t' `appendTrace` tr)

-- | This function is buggy at the moment, it does not work branches
implementTrace :: (Implements t t') => SessionType t' -> Gen (Trace (Session t t'))
implementTrace (B t)       = do
                                t' <- implement t
                                return $ Send (P t') Nothing 
implementTrace (Q t)       = return $ Recv $ Left Nothing
implementTrace (st :| st') = oneof $ [fmap (fmap L) (implementTrace st), fmap (fmap R) (implementTrace st')]
implementTrace (st :& st') = do
                                t  <- implementTrace st
                                t' <- implementTrace st'
                                return $ Recv $ Right (t, t') 
implementTrace (st :. st') = do
                                t  <- implementTrace st
                                t' <- implementTrace st'
                                return $ t `appendTrace` t'
implementTrace End         = return Terminate

instance (Implements t t') => Implements (Trace (Session t t')) (SessionType t') where
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
