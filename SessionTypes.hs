{-# LANGUAGE DeriveFunctor #-}
module SessionTypes where
import JSONType

data SessionType t = B (SessionType t)
                   | Q (SessionType t)
                   | End
                   | (SessionType t) :& (SessionType t)
                   | (SessionType t) :| (SessionType t)
                   | (SessionType t) :. (SessionType t)
                   | Primitive t
                   deriving (Show, Eq, Functor)

data Trace t = Send t
             | Recv (t -> Trace t)
             | Terminate

dual :: SessionType t -> SessionType t
dual (B t)     = Q (dual t)
dual (Q t)     = B (dual t)
dual (t :& t') = (dual t) :| (dual t')
dual (t :| t') = (dual t) :& (dual t')
dual (t :. t') = (dual t) :. (dual t')
dual t         = t

t :: t -> SessionType t
t = Primitive

end :: SessionType t
end = End

(!) :: SessionType t -> SessionType t
(!) = B

(?) :: SessionType t -> SessionType t
(?) = Q
