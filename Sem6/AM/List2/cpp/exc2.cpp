#include <iostream>
#include <vector>
#include <random>
#include <algorithm>
#include <climits>

#include "solvers.hpp"

#define NUM_TRIALS 5

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

    // initialize things
    std::random_device rd;
    std::mt19937 mt{rd()};
    float avg_cost = 0;
    int best_cost = INT_MAX;
    
    for (int i = 0; i < NUM_TRIALS; ++i) {

        // initialize solution
        for (int j = 0; j < n; ++j) {
            solution[j] = j;
        }
        std::shuffle(solution.begin(), solution.end(), mt);

        // tabu search
        int cost = tabuSearch(
            weights, 
            solution,
            52, 
            1434, 
            78
        );

        avg_cost = avg_cost * (i / (i + 1.0f)) + cost / (i + 1.0f);
        best_cost = std::min(best_cost, cost);
    }

    // return results to python process
    std::cout << avg_cost << " " << best_cost << std::endl;
}