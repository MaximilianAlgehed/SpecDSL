{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, FlexibleContexts #-}
-- | This file gives a model of communication between two actors implementing a session
-- | type
module Model where

data Protocol t = Pure t
                | ChooseLeft
                | ChooseRight
                deriving (Show, Functor)

data Interaction t = Got t | Sent t deriving (Show, Functor)
type Log t = [Interaction (Protocol t)]
