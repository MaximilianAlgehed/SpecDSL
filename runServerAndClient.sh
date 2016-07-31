#!/bin/bash

# This is a helper script which first launches the web server and then the web client.
# Press CTRL + C to exit.

cabal run WebServer &
sleep 0.1
cabal run WebClient

wait
