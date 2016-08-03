{-# LANGUAGE MultiParamTypeClasses #-}
module PizzaExample where

import Debug.Trace
import Data.List
import Control.Concurrent
import Control.Concurrent.Chan
import Typeclasses
import Model
import Test.QuickCheck
import SessionTypes hiding (B, L, R)
import JSONType
import LTL hiding (check)
import Run

-- | Pizza toppings datatype
data PizzaToppings = Ham | Cheese | Mushrooms | Olives | Shrimps deriving (Show, Eq, Ord, Enum)

-- | Salad ingredients
data SaladToppings = Feta | Cucumber | Pineapple | SweetCorn | Onion deriving (Show, Eq, Ord, Enum)

-- | The universe of our types
data TypeUniverse = PT | ST | S | I | D | H | L TypeUniverse | E TypeUniverse TypeUniverse deriving (Show, Eq)

-- | A unitype to represent all our types
data Unitype = PiTp PizzaToppings | SaTp SaladToppings | String String | Int Int | Done | Lst [Unitype] | Eith (Either Unitype Unitype) | Hello deriving (Show, Eq)

-- | Mechanical instance, more or less
instance Implements Unitype TypeUniverse where
    implement PT     = fmap PiTp $ oneof $ map return [Ham .. Shrimps]
    implement ST     = fmap SaTp $ oneof $ map return [Feta .. Onion]
    implement S      = fmap String arbitrary 
    implement I      = fmap Int arbitrary
    implement D      = return Done
    implement H      = return Hello
    implement (L t)  = do
                        n <- fmap abs arbitrary
                        lst <- sequence $ replicate n (implement t)
                        return $ Lst lst
    implement (E t t') = do
                            b <- arbitrary
                            if b then
                                fmap Eith $ fmap Left $ implement t
                            else
                                fmap Eith $ fmap Right $  implement t'

-- | Almost mechanical instance
instance Checks TypeUniverse Unitype where
    check H Hello       = True
    check PT (PiTp _)   = True
    check ST (SaTp _)   = True
    check S  (String _) = True
    check I  (Int _)    = True
    check D  Done       = True
    check (L t) (Lst lst) = and [check t x | x <- lst]
    check (E t _) (Eith (Left x)) = check t x
    check (E _ t) (Eith (Right x)) = check t x
    check _ _           = False

-- | The type for ordering pizza
pizzaOrder :: SessionType TypeUniverse
pizzaOrder = (!)H :. addItem :. (pizzaOrder :| finalizeOrder) :. end
    where
        addItem = addPizza :| addSalad
        addPizza = pizzaToppings
        pizzaToppings = (!)PT :. (pizzaToppings :| (!)D)
        addSalad = saladToppings 
        saladToppings = (!)ST :. (saladToppings :| (!)D)
        finalizeOrder = (?) ordered :. (?)cost :. finish

        -- | A list of either a list of pizza toppings or a list of salad toppings
        ordered       = L (E (L PT) (L ST))

        finish = (!)address :. (!)creditCardNumber
        address = S
        creditCardNumber = I
        cost = I

-- | The pizza predicate,
-- | "if I order a pizza, it will be in the list of pizzas at the end"
pizzaPredicate :: LTL (Interaction (Protocol Unitype))
pizzaPredicate = G $ ((Not isPizzaTopping) .& (X (X isPizzaTopping))) .=> X (X (pizzaToppingList []))
    where
        isPizzaTopping = Atomic (\message -> case message of
                                    Sent (Pure (PiTp _)) -> Top
                                    _                    -> Bottom
                                )
        pizzaToppingList lst = Atomic (\message -> case message of
                                            Sent (Pure (PiTp t)) -> X $ X $ pizzaToppingList (t:lst)
                                            _                    -> F $ myPizzaIsInTheList lst
                                      )
        myPizzaIsInTheList lst = Atomic (\message -> case message of
                                            Got (Pure (Lst pizzasAndSalads)) -> fromBool (lst `isIn` pizzasAndSalads)
                                            _                                -> Bottom
                                        )
        isIn lst pas = or [sort lst == sort [x | PiTp x <-  pizza] | Eith (Left (Lst pizza)) <- pas]
