-module(pizzaExample).
-export(main).

main() -> spawn(fun() -> register(p, self()), loop([]) end).
