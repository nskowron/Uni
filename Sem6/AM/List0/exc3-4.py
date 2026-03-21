import tsplib95 as tsp
import matplotlib.pyplot as plt
from prim import prim, dfs
import networkx as nx

filenames = ["dj38.tsp", "qa194.tsp", "uy734.tsp", "wi29.tsp", "zi929.tsp"]

for filename in filenames:
    print(f"Processing file: {filename}")
    problem = tsp.load("data/" + filename)

    n = len(list(problem.get_nodes()))    
    G = problem.get_graph()
    mst = prim(G)

    route = []
    dfs(mst, 1, route)
    route.append(1) # close the cycle

    mst_length = sum(data['weight'] for _, _, data in mst.edges(data=True))
    route_length = sum(problem.get_weight(*edge) for edge in zip(route, route[1:] + route[:1]))

    # results
    print(f"MST length: {mst_length}, Route length: {route_length}")

    plt.figure()
    plt.scatter(*zip(*problem.node_coords.values()), color='black')

    # MST
    for u, v in mst.edges():
        xs = [problem.node_coords[u][0], problem.node_coords[v][0]]
        ys = [problem.node_coords[u][1], problem.node_coords[v][1]]
        plt.plot(xs, ys, color='blue', alpha=0.5)

    # route
    xs = [problem.node_coords[node][0] for node in route]
    ys = [problem.node_coords[node][1] for node in route]
    plt.plot(xs, ys, color='red')

    plt.title(f"MST (blue) and TSP route (red)")
    plt.savefig(f"plots/approx-2/{filename.replace(".tsp", "")}.png")