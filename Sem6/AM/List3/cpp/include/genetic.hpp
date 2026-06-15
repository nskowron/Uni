#pragma once

#include <vector>
#include <random>
#include <barrier>
#include <functional>
#include <utility>

struct Individual {
    std::vector<int> tour;
    int cost;
};

const Individual& tournamentSelection(
    const std::vector<Individual>& population, 
    int tournamentSize, 
    std::mt19937& mt,
    std::uniform_int_distribution<int>& randParent
);

Individual crossoverOX1(
    const std::vector<std::vector<int>>& weights,
    const std::vector<std::pair<int, int>>& neighbors,
    const Individual& parent1,
    const Individual& parent2,
    std::mt19937& mt,
    std::uniform_int_distribution<int>& randInvert
);

int genetic(
    const std::vector<std::vector<int>>& weights,
    std::vector<int>& solution,
    int populationSize = 20,
    int generations = 100,
    double mutationRate = 0.2,
    int tournamentSize = 2
);

void island(
    const std::vector<std::vector<int>>& _weights,
    const std::vector<std::pair<int, int>>& neighbors,
    std::vector<Individual>& _population,
    Individual& _solution,
    int generations,
    int migrationInterval,
    double mutationRate,
    int tournamentSize,
    std::barrier<std::function<void()>>& syncPoint
);

int islands(
    const std::vector<std::vector<int>>& weights,
    std::vector<int>& solution,
    int numIslands = 8,
    int populationSize = 20,
    int generations = 100,
    int migrationInterval = 10,
    double mutationRate = 0.2,
    int tournamentSize = 2,
    int migrationRate = 3
);

int annealing(
    const std::vector<std::vector<int>>& weights, 
    const std::vector<std::pair<int, int>>& neighbors,
    std::vector<int>& solution, 
    double initialTemp = 100.0, 
    double coolingRate = 0.9, 
    int epochs = 100, 
    double stepsPerEpoch = 1.0
);

std::vector<std::pair<int, int>> getInvertNeighbors(int n);

int getSolutionCost(
    const std::vector<std::vector<int>>& weights, 
    const std::vector<int>& solution
);