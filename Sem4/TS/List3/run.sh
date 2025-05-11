#!/bin/bash

# Java stuff
cd Rendering/graph
mvn -q javafx:run &
pid=$!
while ! nc -z -v localhost 2137; do sleep 1; done # wait until server is up
cd ../..

# Haskell stuff
cd Generating
socat TCP:localhost:2137 EXEC:"cabal run -v0" &
cd ..

# Python stuff
cd Simulating
venv/bin/python3 main.py &
cd ..

wait $pid # wait for Java

echo "all's well that ends well"
