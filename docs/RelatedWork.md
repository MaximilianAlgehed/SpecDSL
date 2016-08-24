# Related work

* [Embedding Session types in Haskell](http://homepages.inf.ed.ac.uk/slindley/papers/gvhs-draft-june2016.pdf)

* [Session Types as Intuitionistic Linear Propositions](https://www.cs.cmu.edu/~fp/papers/concur10.pdf)

* [Session Types with (Almost) no Class](http://users.eecs.northwestern.edu/~jesse/pubs/haskell-session-types/session08.pdf)

* [Déjà Fu: A Concurrency Testing Library for Haskell](https://www.barrucadu.co.uk/publications/dejafu-hs15.pdf)
    This paper is concerned with testing
    concurrent Haskell for common concurrency
    bugs like non-determinism, no deadlock,
    and no exceptions. It is not explicitly
    concerned with testing communication between
    two processes. However, it does define a 
    very tiny DSL for checking corectness of
    output that looks a lot like LTL.

* [Types and Subtypes for Client-Server Interactions](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.110.4730&rep=rep1&type=pdf)

* [Talking Bananas](http://homepages.inf.ed.ac.uk/slindley/papers/talking-bananas.pdf)

* [An expressive semantics of mocking](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.714.2744&rep=rep1&type=pdf)
    This paper defines a DSL for setting up
    mocking of dynamic systems with respect
    to some specification. The work is different
    from mine in that the behaviour of the
    unit under test is assumed to not sctrictly
    follow some communication scheme. The work
    is more general, but does not take advantage
    of the body of work available in the session
    types literature.
