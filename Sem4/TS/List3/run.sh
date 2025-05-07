#!/bin/bash

cd Generating
mkfifo node_pipe
cabal run -v0 < node_pipe | tee ../husky | mvn -q -f ../Rendering/graph/pom.xml javafx:run > node_pipe
rm -rf node_pipe
echo "all's well that ends well"
