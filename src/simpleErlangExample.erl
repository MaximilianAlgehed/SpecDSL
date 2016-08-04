-module(simpleErlangExample).
-export([main/0]).

main() ->
        register(p, self()),
        receive
            {Hs, {pure, I}} -> Hs ! {pure, {I, 2*I}}
        end.
