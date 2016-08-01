# Minimum Viable Product

ST = Session Types

* ST -> DSL
    * Branches and Choices may need to be named
      to make the process of specifying predicates
      simpler.

* Predicates -> DSL
    * We need to base this DSL
      on some type of temporal
      logic like LTL.
      Testing is then a case of
      verifiying that
      log |= predicate.
    * The DSL needs to support a slew of high-level
      features to make sure that you don't have to write
      p -> (~q U (q & r)) to express:
      "The first thing that satisfies q after p also
      satisfies r"

* Custom Generators DSL
    * Should just be a case of supplying something
      like "SessionType MyTypeTemplate -> Gen MyTypeThatIWantToTest",
      basically a custom implements function.

* DSL -> Tests

    * ST -> Test
        run :: SessionType t -> Channel t -> IO Bool
        Tests if the other party is fulfilling the contract
        of the SessionType.
        
        This needs to be tested.

    * Predicates + Custom Generators -> Test
        test :: SessionType t -> Predicate t -> Channel t -> IO Bool
        Tests if a given predicate holds with respect to the
        session type over a channel, should also do shrinking etc
        to find a minimum failing example.

    * In the pi-calculus and session types you can send channels with
      a given session type on the channel. We need to implement this
      in our proof of concept. Regardless we need to be able to transmit
      types on the channels. I think we need to write a session type
      protocol.
