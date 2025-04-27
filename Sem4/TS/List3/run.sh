#!/bin/bash

mkfifo json_pipe
mkfifo node_pipe
cd Generating
cabal run -v0 < ../node_pipe > ../json_pipe &
cd ../Rendering/graph
sleep 2
mvn -q javafx:run < ../../json_pipe > ../../node_pipe &&
cd ../..
rm -rf json_pipe
rm -rf node_pipe
echo "all's well that ends well"
