#!/bin/bash

mkfifo json_pipe
cd Generating
cabal run -v0 > ../json_pipe &
cd ../Rendering/graph
mvn -q javafx:run < ../../json_pipe &&
cd ../..
rm -rf json_pipe
echo "all's well that ends well"
