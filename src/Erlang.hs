module Erlang where
import Control.Concurrent.Chan
import System.Process
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

runErlang :: (Erlang r)
          => String -- module name
          -> String -- function name
          -> P Chan (Protocol r)
          -> IO ()
runErlang mod fun ch =
    do
        -- TODO: Spawn erlang node
        -- TODO: forkIO $ Listen to erlang, send fromErlang to the ch
        -- TODO: Listen to the ch, send toErlang to erlang
        -- TODO: we need a mechanism for killing the channel and the process
        (_, _, _, pid) <- createProcess $ (shell $ "erl -name \"erl@127.0.0.1\" -run "++mod++" "++fun) {std_out = NoStream}
        self <- createSelf "haskell@localhost"
        mbox <- createMBox self
        --terminateProcess pid
        return ()
