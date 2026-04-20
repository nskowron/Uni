#include <local_search.hpp>

#include <vector>
#include <random>
#include <algorithm>
#include <memory>

// #include <iostream> // logs --- IGNORE ---

std::vector<LocalSearch::InvertPair> LocalSearch::getInvertNeighbors(int n) {
    std::vector<InvertPair> neighbors;
    for (int i = 1; i < n - 1; ++i) {
        for (int j = i + 1; j < n - 1; ++j) {
            neighbors.push_back({i, j});
        }
    }
    return neighbors;
}

LocalSearch::InvertPair LocalSearch::getBestInvertNeighbor(
    const std::vector<std::vector<int>>& weights, 
    const std::vector<int>& solution, 
    const std::vector<InvertPair>& neighbors
) {
    int n = solution.size(); // for convenience
    InvertPair best_neighbor = {0, 0}; // self
    int best_cost_reduction = 0;
    
    for (const auto& [i, j] : neighbors) {
        int cost_reduction = 
            weights[solution[i]][solution[i - 1]] + 
            weights[solution[j]][solution[j + 1]] - 
            weights[solution[j]][solution[i - 1]] -
            weights[solution[i]][solution[j + 1]];

        if (cost_reduction > best_cost_reduction) {
            best_cost_reduction = cost_reduction;
            best_neighbor = {i, j};
        }
    }
    
    return best_neighbor;
}

int LocalSearch::invertLocalSearch(
    const std::vector<std::vector<int>>& weights, 
    std::vector<int>& solution, 
    int k
) {
    int n = solution.size();
    auto all_neighbors = LocalSearch::getInvertNeighbors(n);
    int steps = 0;

    // std::cerr << "total neighbors: " << all_neighbors.size() << std::endl; // logs --- IGNORE ---
    
    while (true) {
        ++steps;

        auto* neighbors = &all_neighbors;
        if (k < all_neighbors.size()) { // consider only k random neighbors
            std::shuffle(all_neighbors.begin(), all_neighbors.end(), std::mt19937{std::random_device{}()});
            neighbors = new std::vector<InvertPair>(k);
            std::copy(all_neighbors.begin(), all_neighbors.begin() + k, neighbors->begin());

            // std::cerr << "dupa1"; // logs --- IGNORE ---
        }
        
        auto [i, j] = getBestInvertNeighbor(weights, solution, *neighbors);

        if (k < all_neighbors.size()) {
            delete neighbors;

            // std::cerr << "dupa2"; // logs --- IGNORE ---
        }
        
        if (i == j) { // no improvement
            break;
        }
        
        // perform the inversion
        std::reverse(solution.begin() + i, solution.begin() + j + 1);
    }

    return steps;
}

int LocalSearch::getSolutionCost(
    const std::vector<std::vector<int>>& weights, 
    const std::vector<int>& solution
) {
    int cost = 0;
    int n = solution.size();
    for (int i = 0; i < n - 1; ++i) {
        cost += weights[solution[i]][solution[i + 1]];
    }
    cost += weights[solution[n - 1]][solution[0]]; // close the cycle
    return cost;
}