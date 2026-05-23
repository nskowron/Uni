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
    ["./cpp/build/bin/exc1_opt"],
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
rows = []

trial = 0
while True:
    trial += 1
    print(f"Trial {trial}...")
    line = p.stdout.readline()
    if line == "":
        break
    rows.append(line.strip().split())

# save results to csv
with open("opt/annealing.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerow(["temp", "cooling", "epochs", "steps", "cost"])
    writer.writerows(rows)