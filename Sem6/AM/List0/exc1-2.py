import sys
import tsplib95 as tsp
import random
import matplotlib.pyplot as plt

# config
PERMUTATIONS = 1000
GROUP_SIZES = [10, 50, 1000]

filenames = sys.argv[1:]

for filename in filenames:
    # load data
    print(f"Processing file: {filename}")
    problem = tsp.load(filename)
    
    # shuffle nodes for routes
    n = len(list(problem.get_nodes()))
    routes = [random.sample(range(1, n + 1), n) for _ in range(PERMUTATIONS)]
    # close routes and sum distances
    for route in routes:
        route.append(route[0])
    route_lengths = [sum(problem.get_weight(*edge) for edge in zip(route, route[1:] + route[:1])) for route in routes]

    for group_size in GROUP_SIZES:
        # minimum routes of each group
        min_lengths = []
        min_routes = []
        for i in range(0, PERMUTATIONS, group_size):
            group_lengths = route_lengths[i:i+group_size]
            min_length = min(group_lengths)
            min_lengths.append(min_length)
            min_routes.append(routes[i + group_lengths.index(min_length)])

        avg_length = sum(min_lengths) / len(min_lengths)

        # results
        print(f"Group size: {group_size}, Average minimum route length: {avg_length}")

        plt.figure()
        plt.scatter(*zip(*problem.node_coords.values()), color='black')

        for route in min_routes:
            xs = [problem.node_coords[node][0] for node in route]
            ys = [problem.node_coords[node][1] for node in route]
            plt.plot(xs, ys)

        plt.title(f"Minimum circuits for each group ({PERMUTATIONS // group_size} groups)")
        plt.show()