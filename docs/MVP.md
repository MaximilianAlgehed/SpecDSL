# Minimum Viable Product

ST = Session Types

* ST -> DSL
    * Branches and Choices may need to be named
      to make the process of specifying predicates
      simpler.

* Predicates -> DSL
    * Shallow embedding of LTL
      rather than a deep one.

* Custom Generators DSL
    * Should just be a case of supplying something
      like "SessionType MyTypeTemplate -> Gen MyTypeThatIWantToTest",
      basically a custom implements function.
      Custom instances would be nice, but it should be
      enough to provide a wrapper function that sends `implement`
      as an argument.

* DSL -> Tests
