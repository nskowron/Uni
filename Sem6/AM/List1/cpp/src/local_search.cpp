#include <local_search.hpp>

#include <vector>
#include <random>
#include <algorithm>
#include <memory>

std::vector<LocalSearch::InvertPair> LocalSearch::getInvertNeighbors(int n) {
    std::vector<InvertPair> neighbors;
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
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
            weights[solution[i]][solution[(i - 1 + n) % n]] + 
            weights[solution[j]][solution[(j + 1) % n]] - 
            weights[solution[j]][solution[(i - 1 + n) % n]] -
            weights[solution[i]][solution[(j + 1) % n]];

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
    
    while (true) {
        ++steps;

        auto* neighbors = &all_neighbors;
        if (k < solution.size()) { // consider only k random neighbors
            std::shuffle(all_neighbors.begin(), all_neighbors.end(), std::mt19937{std::random_device{}()});
            neighbors = new std::vector<InvertPair>(k);
            std::copy(all_neighbors.begin(), all_neighbors.begin() + k, neighbors->begin());
        }
        
        InvertPair best_neighbor = getBestInvertNeighbor(weights, solution, *neighbors);

        if (k < solution.size()) {
            delete neighbors;
        }
        
        if (best_neighbor.i == best_neighbor.j) { // no improvement
            break;
        }
        
        // perform the inversion
        std::reverse(solution.begin() + best_neighbor.i, solution.begin() + best_neighbor.j + 1);
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