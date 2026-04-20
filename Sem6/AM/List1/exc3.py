import tsplib95 as tsp
import subprocess
import random

from prim import prim, dfs

filenames = ["wi29.tsp", "dj38.tsp", "qa194.tsp", "uy734.tsp", "zi929.tsp"]

for filename in filenames:
    # load data
    print(f"Processing file: {filename}")
    problem = tsp.load("data/" + filename)
    
    n = len(list(problem.get_nodes()))

    # open cpp process
    p = subprocess.Popen(
        ["./cpp/build/bin/exc3"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True
    )

    # stream data to cpp
    p.stdin.write(f"{n}\n")
    for i in range(1, n + 1):
        for j in range(i, n + 1):
            p.stdin.write(f"{problem.get_weight(i, j)}\n")
    p.stdin.flush()

    print("Data sent to C++ process.")

    # make mst
    mst = prim(problem.get_graph())
    print(f"mst weight: {sum(data['weight'] for _, _, data in mst.edges(data=True))}")

    # stream dfs route
    for i in range(5):
        v = random.randint(1, n)
        route = []
        dfs(mst, v, route)
        for j in range(len(route)):
            p.stdin.write(f"{route[j] - 1}\n")
        p.stdin.flush()

    # receive results from cpp
    results = p.stdout.readline().strip().split()
    best_cost = int(results[0])
    avg_steps = float(results[1])
    avg_cost = float(results[2])
    best_solution = [int(x) for x in p.stdout.readline().strip().split()]

    # present results
    print(f"Best cost: {best_cost}")
    print(f"Average steps: {avg_steps}")
    print(f"Average cost: {avg_cost}")