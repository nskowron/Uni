#include "local_search.hpp"

#include <iostream>
#include <vector>
#include <random>
#include <algorithm>

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

        // preallocate solution and nodes
        std::vector<int> solution(n);
        std::vector<int> nodes(n);
        for (int j = 0; j < n; ++j) {
            nodes[j] = j;
        }
        
        // exercise stuff
        std::vector<int> best_solution = nodes;
        int best_cost = LocalSearch::getSolutionCost(weights, best_solution);
        float avg_cost = 0;
        float avg_steps = 0;

        // run local search for n random permutations
        for (int i = 0; i < 5; ++i) {
            std::cerr << "iteration " << i + 1 << std::endl; // logs --- IGNORE ---
            // new permutation
            solution = nodes;
            std::shuffle(solution.begin(), solution.end(), std::mt19937{std::random_device{}()});

            // local search
            int steps = LocalSearch::invertLocalSearch(weights, solution, n);
            
            // results
            avg_steps = avg_steps * (i / (i + 1.0f)) + steps / (i + 1.0f);
            int cost = LocalSearch::getSolutionCost(weights, solution);
            avg_cost = avg_cost * (i / (i + 1.0f)) + cost / (i + 1.0f);
            if (cost < best_cost) {
                best_cost = cost;
                best_solution = solution;
            }
        }

        // return results to python process
        std::cout << best_cost << " " << avg_steps << " " << avg_cost << std::endl;
        for (int node : best_solution) {
            std::cout << node + 1 << " ";
        }
        std::cout << best_solution[n - 1] << std::endl << std::flush;
    }
}