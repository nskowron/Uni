#pragma once 

#include <vector>
#include <utility>


int annealing(
    const std::vector<std::vector<int>>& weights, 
    std::vector<int>& solution, 
    double initialTemp = 100.0, 
    double coolingRate = 0.95, 
    int epochs = 1000, 
    int stepsPerEpoch = 100
);

int tabuSearch(
    const std::vector<std::vector<int>>& weights, 
    std::vector<int>& solution, 
    const int tabuLength = 500, 
    const int maxIterWithoutImprovement = 500, 
    const int neighborhoodSize = 10
);

int getSolutionCost(
    const std::vector<std::vector<int>>& weights, 
    const std::vector<int>& solution
);

std::vector<std::pair<int, int>> getInvertNeighbors(int n);