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
    Lst = (if
                I > 0 -> [I|XS];
                true  -> [XS]
           end),
    receive 
        {_, chooseLeft} -> loop(Lst);
        {_, chooseRight} -> finish(Lst)
    end.

finish(XS) ->
    receive 
        {Hs, {pure, requestBasket}} -> Hs ! {pure, {yourBasket, XS}}
    end,
    exit(done).
