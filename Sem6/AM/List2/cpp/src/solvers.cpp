#include <vector>
#include <random>
#include <algorithm>
#include <deque>

#include "solvers.hpp"

int annealing(
    const std::vector<std::vector<int>>& weights, 
    std::vector<int>& solution, 
    double initialTemp, 
    double coolingRate, 
    int epochs, 
    int stepsPerEpoch
) {
    // initializing
    const int n = solution.size();
    const std::vector<std::pair<int, int>> neighbors = getInvertNeighbors(n);
    int currCost = getSolutionCost(weights, solution);
    int bestCost = currCost;
    std::vector<int> bestSolution = solution;
    double temp = initialTemp;

    // random number generators
    std::random_device rd;
    std::mt19937 mt{rd()};
    std::uniform_int_distribution randNeighbor{0, (int)neighbors.size() - 1};
    std::uniform_real_distribution<double> randProb(0.0, 1.0);  

    // main loop
    for (int epoch = 0; epoch < epochs; ++epoch) {
        for (int trial = 0; trial < stepsPerEpoch; ++trial) {

            // generate a random invert neighbor
            auto [i, j] = neighbors[randNeighbor(mt)];

            // calculate the neighbors cost reduction
            int cost_reduction = 
                weights[solution[i]][solution[(i - 1 + n) % n]] + 
                weights[solution[j]][solution[(j + 1) % n]] - 
                weights[solution[j]][solution[(i - 1 + n) % n]] -
                weights[solution[i]][solution[(j + 1) % n]];          

            // accept the new solution if its better or with a probability
            if (cost_reduction > 0 || exp(cost_reduction / temp) > randProb(mt)) {
                std::reverse(solution.begin() + i, solution.begin() + j + 1);
                currCost -= cost_reduction;

                // update best solution
                if (currCost < bestCost) {
                    bestCost = currCost;
                    bestSolution = solution;
                }
            }
        }
        temp *= coolingRate;
    }

    solution = bestSolution;
    return bestCost;
}

int tabuSearch(
    const std::vector<std::vector<int>>& weights, 
    std::vector<int>& solution, 
    int tabuLength, 
    int maxIterWithoutImprovement, 
    int neighborhoodSize
) {
    // initializing
    const int n = solution.size();
    const std::vector<std::pair<int, int>> neighbors = getInvertNeighbors(n);
    int currCost = getSolutionCost(weights, solution);
    int bestCost = currCost;
    std::vector<int> bestSolution = solution;
    int iter = 0;
    int iterWithoutImprovement = 0;
    std::deque<std::pair<int, int>> tabuList{(size_t)tabuLength, {0, 0}};

    // random number generator
    std::random_device rd;
    std::mt19937 mt{rd()};
    std::uniform_int_distribution randNeighbor{0, (int)neighbors.size() - 1};

    while(iterWithoutImprovement < maxIterWithoutImprovement) {
        ++iter;
        int bestNeighborCostReduction = 0;
        std::pair<int, int> bestNeighbor = {-1, -1};

        for (int k = 0; k < neighborhoodSize; k++) {
            
            // generate a random invert neighbor
            auto [i, j] = neighbors[randNeighbor(mt)];

            // calculate the neighbors cost reduction
            int cost_reduction = 
                weights[solution[i]][solution[(i - 1 + n) % n]] + 
                weights[solution[j]][solution[(j + 1) % n]] - 
                weights[solution[j]][solution[(i - 1 + n) % n]] -
                weights[solution[i]][solution[(j + 1) % n]]; 

            // check if neighbor is tabu
            bool isTabu = false;
            for (const auto& [tabu_i, tabu_j] : tabuList) {
                if (tabu_i == i && tabu_j == j) {
                    isTabu = true;
                    break;
                }
            }

            // aspiration criterion
            if (isTabu && currCost - cost_reduction >= bestCost)
                continue;

            if (cost_reduction > bestNeighborCostReduction) {
                bestNeighborCostReduction = cost_reduction;
                bestNeighbor = {i, j};
            }
        }

        auto [i, j] = bestNeighbor;

        // in case no neighbor was found
        if (i == -1) break;

        // accept the best neighbor
        std::reverse(solution.begin() + i, solution.begin() + j + 1);
        currCost -= bestNeighborCostReduction;

        // update best solution
        if (currCost < bestCost) {
            bestCost = currCost;
            bestSolution = solution;
            iterWithoutImprovement = 0;
        } else {
            iterWithoutImprovement++;
        }

        // update tabu list
        tabuList.pop_front();
        tabuList.push_back({i, j});
    }

    solution = bestSolution;
    return bestCost;
}

int getSolutionCost(
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

std::vector<std::pair<int, int>> getInvertNeighbors(int n) {
    std::vector<std::pair<int, int>> neighbors;
    for (int i = 0; i < n - 1; ++i) {
        for (int j = i + 1; j < n; ++j) {
            if (i == 0 && j == n - 1) continue;
            neighbors.push_back({i, j});
        }
    }
    return neighbors;
}