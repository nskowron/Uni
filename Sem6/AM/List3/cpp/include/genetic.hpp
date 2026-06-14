#pragma once

#include <vector>
#include <random>
#include <barrier>
#include <functional>

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

int genetic(
    const std::vector<std::vector<int>>& weights,
    std::vector<int>& solution,
    int populationSize = 500,
    int generations = 40,
    double mutationRate = 0.1,
    double tournamentSize = 0.05
);

void island(
    const std::vector<std::vector<int>>& _weights,
    std::vector<Individual>& _population,
    Individual& _solution,
    int generations,
    int migrationInterval,
    double mutationRate,
    double tournamentSize,
    std::barrier<std::function<void()>>& syncPoint
);

int islands(
    const std::vector<std::vector<int>>& weights,
    std::vector<int>& solution,
    int numIslands,
    int populationSize,
    int generations,
    int migrationInterval,
    double mutationRate,
    double tournamentSize,
    double migrationRate
);

int getSolutionCost(
    const std::vector<std::vector<int>>& weights, 
    const std::vector<int>& solution
);