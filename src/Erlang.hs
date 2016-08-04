module Erlang where
import Control.Concurrent
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
        (_, _, _, pid) <- createProcess $ (shell $ "erl -name \"erl@127.0.0.1\" -run "++mod++" "++fun) {std_out = NoStream}
        self <- createSelf "haskell@localhost"
        mbox <- createMBox self
        id1 <- forkIO $ erlangLoop ch mbox
        id2 <- forkIO $ haskellLoop ch mbox
        waitToBeKilled ch >> (finish () id1 id2)
        return ()
    where
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
                mboxSend mbox (Long "erl" "127.0.0.1") (Right "p") (mboxSelf mbox, m)
                haskellLoop ch mbox
