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

data Session typeReal typeTemplate  = Channel (SessionType typeTemplate)
                                    | P (Session typeReal)
                                    | L (Session typeReal)
                                    | R (Session typeReal)
                                    | End
                                    deriving (Show)

data Trace t = Send t (Trace t)
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

v :: Name -> SessionType t
v = Var

end :: SessionType t
end = End

(!) :: SessionType t -> SessionType t
(!) = B

(?) :: SessionType t -> SessionType t
(?) = Q
