#include <vector>
#include <random>
#include <algorithm>
#include <barrier>
#include <thread>
#include <iterator>
#include <functional>
#include <utility>

#include "genetic.hpp"

const Individual& tournamentSelection(
    const std::vector<Individual>& population, 
    int tournamentSize, 
    std::mt19937& mt,
    std::uniform_int_distribution<int>& randParent
) {
    int bestIdx = randParent(mt);
    for (int i = 1; i < tournamentSize; ++i) {
        int idx = randParent(mt);
        if (population[idx].cost < population[bestIdx].cost) {
            bestIdx = idx;
        }
    }
    return population[bestIdx];
}

Individual crossoverOX1(
    const std::vector<std::vector<int>>& weights,
    const std::vector<std::pair<int, int>>& neighbors,
    const Individual& parent1,
    const Individual& parent2,
    std::mt19937& mt,
    std::uniform_int_distribution<int>& randInvert
) {
    const int n = parent1.tour.size();

    Individual child;
    child.tour.resize(n, -1);

    std::vector<bool> visited(n, false);

    // random segment
    auto [a, b] = neighbors[randInvert(mt)];

    // copy segment from parent1
    for (int i = a; i <= b; i++) {
        child.tour[i] = parent1.tour[i];
        visited[parent1.tour[i]] = true;
    }

    // fill remaining positions from parent2
    int childPos = (b + 1) % n;
    int parentPos = (b + 1) % n;

    while (true) {
        int city = parent2.tour[parentPos];

        if (!visited[city]) {
            child.tour[childPos] = city;
            visited[city] = true;
            childPos = (childPos + 1) % n;
        }

        parentPos = (parentPos + 1) % n;

        if (parentPos == (b + 1) % n)
            break;
    }

    return child;
}

int genetic(
    const std::vector<std::vector<int>>& weights,
    std::vector<int>& solution,
    int populationSize,
    int generations,
    double mutationRate,
    int tournamentSize
) {
    const int n = solution.size();
    std::vector<std::pair<int, int>> neighbors = getInvertNeighbors(n);
    Individual bestIndividual{solution, getSolutionCost(weights, solution)};
    std::vector<Individual> population(populationSize - 1);

    // random number generators
    std::random_device rd;
    std::mt19937 mt{rd()};
    std::uniform_int_distribution<int> randInvert(0, neighbors.size() - 1);
    std::uniform_int_distribution<int> randParent(0, populationSize - 1);
    std::uniform_real_distribution<double> randProb(0.0, 1.0);

    // initialize random population
    for (auto& ind : population) {
        ind.tour = solution;
        std::shuffle(ind.tour.begin(), ind.tour.end(), mt);
        ind.cost = getSolutionCost(weights, ind.tour);
    }
    population.push_back(bestIndividual);

    // update best individual
    bestIndividual = *std::min_element(population.begin(), population.end(), 
        [](const Individual& a, const Individual& b) { return a.cost < b.cost; });

    for (int gen = 0; gen < generations; ++gen) {

        // next generation placeholder
        std::vector<Individual> nextGeneration;
        nextGeneration.reserve(populationSize);

        // keep the best individual (elitism)
        nextGeneration.push_back(
            *std::min_element(population.begin(), population.end(), 
                [](const Individual& a, const Individual& b) { return a.cost < b.cost; })
        );

        // generate rest of the population
        while (nextGeneration.size() < populationSize) {
            
            // tournament selection
            const Individual& parent1 = tournamentSelection(population, tournamentSize, mt, randParent);
            const Individual& parent2 = tournamentSelection(population, tournamentSize, mt, randParent);

            // crossover
            Individual child = crossoverOX1(weights, neighbors, parent1, parent2, mt, randInvert);

            // mutation (inverse)
            if (randProb(mt) < mutationRate) {
                auto [a, b] = neighbors[randInvert(mt)];
                std::reverse(child.tour.begin() + a, child.tour.begin() + b + 1);
            }

            // local search step
            child.cost = annealing(
                weights,
                neighbors,
                child.tour
            );
            nextGeneration.push_back(child);
        }

        population = std::move(nextGeneration);

        // update best individual
        bestIndividual = *std::min_element(population.begin(), population.end(), 
            [](const Individual& a, const Individual& b) { return a.cost < b.cost; });
    }

    solution = bestIndividual.tour;
    return bestIndividual.cost;
}

void island(
    const std::vector<std::vector<int>>& weights,
    const std::vector<std::pair<int, int>>& neighbors,
    std::vector<Individual>& population,
    Individual& solution,
    int generations,
    int migrationInterval,
    double mutationRate,
    int tournamentSize,
    std::barrier<std::function<void()>>& syncPoint
) {
    const int n = solution.tour.size();
    Individual bestIndividual = *std::min_element(population.begin(), population.end(), 
        [](const Individual& a, const Individual& b) { return a.cost < b.cost; });

    // random number generators
    std::random_device rd;
    std::mt19937 mt{rd()};
    std::uniform_int_distribution<int> randInvert(0, neighbors.size() - 1);
    std::uniform_int_distribution<int> randParent(0, population.size() - 1);
    std::uniform_real_distribution<double> randProb(0.0, 1.0);

    for (int gen = 1; gen <= generations; ++gen) {

        // next generation placeholder
        std::vector<Individual> nextGeneration;
        nextGeneration.reserve(population.size());

        // keep the best individual (elitism)
        nextGeneration.push_back(
            *std::min_element(population.begin(), population.end(), 
                [](const Individual& a, const Individual& b) { return a.cost < b.cost; })
        );

        // generate rest of the population
        while (nextGeneration.size() < population.size()) {
            
            // tournament selection
            const Individual& parent1 = tournamentSelection(population, tournamentSize, mt, randParent);
            const Individual& parent2 = tournamentSelection(population, tournamentSize, mt, randParent);

            // crossover
            Individual child = crossoverOX1(weights, neighbors, parent1, parent2, mt, randInvert);

            // mutation (inverse)
            if (randProb(mt) < mutationRate) {
                auto [a, b] = neighbors[randInvert(mt)];
                std::reverse(child.tour.begin() + a, child.tour.begin() + b + 1);
            }

            child.cost = annealing(
                weights,
                neighbors,
                child.tour
            );
            nextGeneration.push_back(child);
        }

        population = std::move(nextGeneration);

        // update best individual
        bestIndividual = *std::min_element(population.begin(), population.end(), 
            [](const Individual& a, const Individual& b) { return a.cost < b.cost; });

        // synchronize with other islands
        if (gen % migrationInterval == 0) {
            syncPoint.arrive_and_wait();
        }
    }

    solution = bestIndividual;
}

int islands(
    const std::vector<std::vector<int>>& weights,
    std::vector<int>& solution,
    int numIslands,
    int populationSize,
    int generations,
    int migrationInterval,
    double mutationRate,
    int tournamentSize,
    int migrationSize
) {
    const int n = solution.size();
    std::vector<std::pair<int, int>> neighbors = getInvertNeighbors(n);
    int solutionCost = getSolutionCost(weights, solution);
    std::vector<Individual> islandsSolutions(numIslands, {solution, solutionCost});
    std::vector<std::vector<Individual>> islandsPopulations(numIslands, std::vector<Individual>(populationSize - 1));
    
    // random number generators
    std::random_device rd;
    std::mt19937 mt{rd()};
    
    // initialize random populations for each island
    for (auto& population : islandsPopulations) {
        for (auto& ind : population) {
            ind.tour = solution;
            std::shuffle(ind.tour.begin(), ind.tour.end(), mt);
            ind.cost = getSolutionCost(weights, ind.tour);
        }
        population.push_back({solution, getSolutionCost(weights, solution)});
    }

    // barrier for synchronization
    std::barrier<std::function<void()>> syncPoint(
        numIslands,
        [&]() {
            for (auto& population : islandsPopulations) {
                std::sort(population.begin(), population.end(), 
                    [](const Individual& a, const Individual& b) { return a.cost < b.cost; });
            }
            // migrate best and replace worst
            std::vector<Individual> migrants;
            migrants.reserve(migrationSize);
            std::copy(
                islandsPopulations[numIslands - 1].begin(), 
                islandsPopulations[numIslands - 1].begin() + migrationSize + 1, 
                std::back_inserter(migrants)
            );
            for (auto& population : islandsPopulations) {
                std::copy(migrants.begin(), migrants.end(), population.end() - migrationSize - 1);
                std::copy(population.begin(), population.begin() + migrationSize + 1, migrants.begin());
            }
        }
    );

    // launch threads for each island
    std::vector<std::thread> threads;
    for (int i = 0; i < numIslands; ++i) {
        threads.emplace_back(
            island, 
            std::cref(weights), 
            std::cref(neighbors),
            std::ref(islandsPopulations[i]), 
            std::ref(islandsSolutions[i]), 
            generations, 
            migrationInterval, 
            mutationRate, 
            tournamentSize, 
            std::ref(syncPoint)
        );
    }

    // wait for all threads to finish
    for (auto& t : threads) {
        t.join();
    }

    // find best solution among all islands
    Individual bestIndividual = *std::min_element(
        islandsSolutions.begin(),
        islandsSolutions.end(),
        [](const Individual& a, const Individual& b) { return a.cost < b.cost; }
    );

    solution = bestIndividual.tour;
    return bestIndividual.cost;
}

int annealing(
    const std::vector<std::vector<int>>& weights, 
    const std::vector<std::pair<int, int>>& neighbors,
    std::vector<int>& solution, 
    double initialTemp, 
    double coolingRate, 
    int epochs, 
    double stepsPerEpoch
) {
    // initializing
    const int n = solution.size();
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
        for (int trial = 0; trial < stepsPerEpoch * n; ++trial) {

            // generate a random invert neighbor
            auto [i, j] = neighbors[randNeighbor(mt)];

            // calculate the neighbors cost reduction
            int cost_reduction = 
                weights[solution[i]][solution[(i - 1 + n) % n]] + 
                weights[solution[j]][solution[(j + 1) % n]] - 
                weights[solution[j]][solution[(i - 1 + n) % n]] -
                weights[solution[i]][solution[(j + 1) % n]];          

            // accept the new solution if its better or with a probability
            if (cost_reduction > 0 || std::exp(cost_reduction / temp) > randProb(mt)) {
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