#pragma once

#include <vector>

namespace LocalSearch {
    struct InvertPair {
        int i;
        int j;
    };

    std::vector<InvertPair> getInvertNeighbors(int n);

    InvertPair getBestInvertNeighbor(
        const std::vector<std::vector<int>>& weights, 
        const std::vector<int>& solution, 
        const std::vector<InvertPair>& neighbors
    );

    int invertLocalSearch(
        const std::vector<std::vector<int>>& weights, 
        std::vector<int>& solution, 
        int k // number of neighbors to consider at each step
    );
};