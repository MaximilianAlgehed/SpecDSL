-module(pizzaExample).
-export(main).

main() -> spawn(fun() -> register(p, self()), loop([]) end).

loop(Items) ->
    recieve
        {Hs, {pure, hello}} -> Item = getItem(),
                               recieve
                                    {Hs, chooseLeft} -> loop([Item|Items]);
                                    {Hs, chooseRight} -> finish(Hs, [Item|Items])
                               end
    end.

finish(Hs, Items) ->
    Hs ! Items,
    Hs ! 1000,
    recieve
        _ -> pass
    end,
    recieve
        _ -> pass
    end.
