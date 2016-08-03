module Erlang where
import Model
import Foreign.Erlang

-- | So that we can talk to Erlang!
instance (Erlang t) => Erlang (Protocol t) where
    toErlang (Pure t) = ErlTuple [ErlAtom "pure", toErlang t]
    toErlang ChooseLeft = ErlAtom "chooseLeft"
    toErlang ChooseRight = ErlAtom "chooseRight"

    fromErlang (ErlTuple [ErlAtom "pure", t]) = Pure (fromErlang t)
    fromErlang (ErlAtom "chooseLeft") = ChooseLeft
    fromErlang (ErlAtom "chooseRight") = ChooseRight
