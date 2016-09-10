## From Koen

* Weak LTL

* Move interperate in to the ST type
    * Issue:
        The interpertation

        ```haskell
            data ST a where
                Bang :: Gen a -> (a -> ST b) -> ST a
                ...
        ```

        Is quite strange because the
        function (a -> ST b) means the type
        is not static, not quite what we want,
        or is it?

        Two questions arise:
            1. Is this what we want?
            2. How can we have a static session type
               with dynamic behaviour?
    * Issue:
        Is there a way to combine the behaviour
        and the specification in a reasonable way?

        ```haskell
        data ST a where
            Bang :: Gen a -> (a -> ST b) -> ST a
            Ques :: (a -> Bool) -> (a -> ST b) -> ST a
            ...
        ```

        The issue with this is that the function

        ```haskell
        dual :: ST a -> ST a
        dual (Ques pred cont) = Bang (arbitrary `suchThat` pred) (dual . cont)
        dual (Bang gen cont)  = Ques (extractPredicate gen) (dual . cont)
        ...

        extractPredicate :: Gen a -> a -> Bool
        extractPredicate gen a = a `canBeGeneratedBy` gen
        ```

        The evident problem is how to generate the function
        "canBeGeneratedBy".

* Real APIs
