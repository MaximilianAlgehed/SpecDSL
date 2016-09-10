## From Koen

* Weak LTL

* Move interperate in to the ST type
    * Issue:
        The interpertation
        data ST a where
             Bang :: Gen a -> (a -> ST b) -> ST a
             ...
        Is quite strange because the
        function (a -> ST b) means the type
        is not static, not quite what we want,
        or is it?

        Two questions arise:
            1. Is this what we want?
            2. How can we have a static session type
               with dynamic behaviour?

* Real APIs
