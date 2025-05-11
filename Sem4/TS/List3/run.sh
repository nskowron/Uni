#!/bin/bash

# Java stuff
cd Rendering/graph
mvn -q javafx:run &
while ! nc -z -v localhost 2137; do sleep 1; done

# Haskell stuff
cd ../../Generating
socat TCP:localhost:2137 EXEC:"cabal run -v0" &&

# Python stuff

rm -rf node_pipe
echo "all's well that ends well"
