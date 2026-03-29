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
        ["./local_search"],
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

    # receive ACK
    ack = p.stdout.readline()
    print(ack)