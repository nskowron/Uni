#!/bin/bash

# Java stuff
cd Rendering/graph
mvn -q javafx:run &
sleep 5 &&

# Haskell stuff
cd ../../Generating
socat TCP:localhost:2137 EXEC:"cabal run -v0" &&


# Python stuff

rm -rf node_pipe
echo "all's well that ends well"
