#!/bin/bash

# Java stuff
cd Rendering/graph
mvn -q javafx:run &
pid_j=$!
cd ../..

while ! nc -z -v localhost 2137; do sleep 1; done # wait until server is up

# Haskell stuff
cd Generating
socat TCP:localhost:2137 EXEC:"cabal run -v0" &
pid_h=$!
cd ..

# Python stuff
cd Simulating
venv/bin/python3 main.py &
pid_p=$!
cd ..

wait $pid_j # wait for Java
kill -9 $pid_h
kill -9 $pid_p

echo "all's well that ends well"
