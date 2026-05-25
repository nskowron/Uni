#include <iostream>
#include <vector>
#include <random>
#include <algorithm>

#include "solvers.hpp"

#define NUM_TRIALS 500
#define NUM_TRIES_PER_TRIAL 3

int main() {
    int n;
    std::cin >> n;

    // read weights
    std::vector<std::vector<int>> weights(n, std::vector<int>(n));
    for (int i = 0; i < n; ++i) {
        for (int j = i; j < n; ++j) {
            std::cin >> weights[i][j];
            weights[j][i] = weights[i][j];
        }
    }

    // preallocate solution
    std::vector<int> solution(n);

    // random number generators
    std::random_device rd;
    std::mt19937 mt{rd()};
    std::uniform_int_distribution randTabuLength(50, 5000);
    std::uniform_int_distribution randIter(50, 5000);
    std::uniform_real_distribution randNeighbourhood(0.0, 1.0);
    
    for (int trial = 0; trial < NUM_TRIALS; ++trial) {

        // initialize parameters
        int tabuLength = randTabuLength(mt);
        int iter = randIter(mt);
        double neighbourhood = randNeighbourhood(mt);
        float avg_cost = 0;

        // run tabu search with random parameters
        for (int i = 0; i < NUM_TRIES_PER_TRIAL; ++i) {
            // new permutation
            for (int i = 0; i < n; ++i) {
                solution[i] = i;
            }
            std::shuffle(solution.begin(), solution.end(), mt);

            // tabu search
            int cost = tabuSearch(
                weights,
                solution,
                tabuLength,
                iter,
                neighbourhood
            );

            // results
            avg_cost = avg_cost * (i / (i + 1.0f)) + cost / (i + 1.0f);
        }

        // return results to python process
        std::cout << tabuLength << " " << iter << " " << neighbourhood << " " << avg_cost << std::endl;
    }
}