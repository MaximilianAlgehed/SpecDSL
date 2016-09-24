-module(erlangBookExample).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() -> register(p, self()),
       loop([]).

loop(XS) ->
    receive 
        {_, {pure, {book, I}}} -> continue(I, XS)
    end.

continue(I, XS) ->
    Lst = [I|XS],
    receive 
        {_, chooseLeft} -> loop(Lst);
        {_, chooseRight} -> finish(Lst)
    end.

finish(XS) ->
    receive 
        {Hs, {pure, requestBasket}} -> Hs ! {pure, {yourBasket, XS}}
    end,
    receive
        {_, chooseLeft} -> loop(XS);
        {_, chooseRight} -> exit(done)
    end.
