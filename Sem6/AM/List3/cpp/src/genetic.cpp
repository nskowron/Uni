#include <vector>
#include <random>
#include <algorithm>

struct Individual {
    std::vector<int> tour;
    int cost;
};

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
    const Individual& parent1, 
    const Individual& parent2, 
    std::mt19937& mt, 
    std::uniform_int_distribution<int>& randGene
) {
    const int n = parent1.tour.size();
    Individual child;
    child.tour.reserve(n);
    std::vector<bool> visited(n, false);
    
    // get a random segment
    int a = randGene(mt);
    int b = randGene(mt);
    if (a > b) std::swap(a, b);

    // copy segment from parent 1
    for (int i = 0; a + i <= b; ++i) {
        int city = parent1.tour[a + i];
        child.tour.push_back(city);
        visited[city] = true;
    }

    // fill remaining from parent 2
    for (int i = 0; i < n; ++i) {
        int city = parent2.tour[i];
        if (!visited[city]) {
            child.tour.push_back(city);
        }
    }

    return child;
}

int geneticAlgorithm(
    const std::vector<std::vector<int>>& weights,
    std::vector<int>& solution,
    int populationSize,
    int generations,
    double mutationRate,
    int tournamentSize
) {
    const int n = solution.size();
    Individual bestIndividual{solution, getSolutionCost(weights, solution)};
    std::vector<Individual> population(populationSize - 1);

    // random number generators
    std::random_device rd;
    std::mt19937 mt{rd()};
    std::uniform_int_distribution<int> randGene(0, n - 1);
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
            Individual child = crossoverOX1(weights, parent1, parent2, mt, randGene);

            // mutation (inverse)
            if (randProb(mt) < mutationRate) {
                int a = randGene(mt);
                int b = randGene(mt);
                if (a > b) std::swap(a, b);
                std::reverse(child.tour.begin() + a, child.tour.begin() + b + 1);
            }

            child.cost = getSolutionCost(weights, child.tour);
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