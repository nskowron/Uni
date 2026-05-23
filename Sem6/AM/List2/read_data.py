import tsplib95 as tsp
import csv
import subprocess

filename = "zi929.tsp"

# load data
print(f"Processing file: {filename}")
problem = tsp.load("../List1/data/" + filename)

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
avg_cost = float(results[0])
best_cost = int(results[1])

print(f"Average cost: {avg_cost}")
print(f"Best cost: {best_cost}")