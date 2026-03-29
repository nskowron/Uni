#include "local_search.hpp"

#include <iostream>
#include <vector>

int main() {
    int n;

    while (std::cin >> n) { // for every file read n
        // read weights
        std::vector<std::vector<int>> weights(n, std::vector<int>(n));
        for (int i = 0; i < n; ++i) {
            for (int j = i; j < n; ++j) {
                std::cin >> weights[i][j];
                weights[j][i] = weights[i][j];
            }
        }

        // run local search for n random permutations
        for (int i = 0; i < n; ++i) {
    }
}