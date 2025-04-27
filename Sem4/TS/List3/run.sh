#!/bin/bash

# mkfifo json_pipe
mkfifo node_pipe
cd Generating
cabal run -v0 < ../node_pipe | tee ../husky | mvn -q -f ../Rendering/graph/pom.xml javafx:run | tee ../node_pipe ../tea
#cd ../Rendering/graph
cd ..
rm -rf node_pipe
echo "all's well that ends well"
