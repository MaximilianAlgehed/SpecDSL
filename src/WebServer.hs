{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $ do
  get "/" $ do
    html "Good morning, I am indeed a web server."
