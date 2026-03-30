import tsplib95 as tsp
import matplotlib.pyplot as plt
import subprocess

filenames = ["ca4663.tsp"]

for filename in filenames:
    # load data
    print(f"Processing file: {filename}")
    problem = tsp.load("data/" + filename)
    
    n = len(list(problem.get_nodes()))

    # open cpp process
    p = subprocess.Popen(
        ["./cpp/build/bin/exc1"],
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

    # receive results from cpp
    results = p.stdout.readline().strip().split()
    best_cost = int(results[0])
    avg_steps = float(results[1])
    best_solution = [int(x) for x in p.stdout.readline().strip().split()]

    # present results
    print(f"Best cost: {best_cost}")
    print(f"Average steps: {avg_steps}")
    print(f"Best solution: {best_solution}")

    plt.figure()
    plt.scatter(*zip(*problem.node_coords.values()), color='black')

    xs = [problem.node_coords[node][0] for node in best_solution]
    ys = [problem.node_coords[node][1] for node in best_solution]
    plt.plot(xs, ys)

    plt.title(f"{filename} - Best Solution Graphical Representation")
    plt.savefig(f"plots/{filename.replace(".tsp", ".png")}")