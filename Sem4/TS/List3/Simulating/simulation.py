import networkx as nx
import random

def simulate(capacities, intensities) -> float:
    G = create_graph(capacities)
    simulations = 50
    successful = 0
    for _ in range(simulations):
        for u, v in G.edges(): # clear graph
            G[u][v]['intensity'] = 0
        if simulate_random_failures(G, intensities, 0.1):
            successful += 1
    return successful / simulations

def create_graph(capacities):
    G = nx.DiGraph()
    n = capacities.shape[0]
    for i in range(n):
        G.add_node(i)
        for j in range(n):
            if capacities[i, j] > 0:
                G.add_edge(i, j, capacity=capacities[i, j])
    return G

def simulate_random_failures(G, intensities, T):
    broken = [(u, v) for u, v in G.edges() if random.random() > 1]
    brokie = nx.subgraph_view(G, filter_edge=lambda u, v: (u, v) not in broken)
    return mean_time_delay(brokie, intensities) <= T

def mean_time_delay(G, intensities):
    n = intensities.shape[0]
    N = 0
    for i in range(n): # reroute whole flow into graph
        for j in range(n):
            flow = intensities[i, j]
            if flow > 0:
                N += flow
                success = reroute_flow(G, i, j, flow)
                if not success:
                    print("Failed to reroute flow")
                    return float('inf')
    if N == 0:
        return 0
    # calculate mean time delay
    result = 0
    for u, v in G.edges():
        if G[u][v]['capacity'] - G[u][v]['intensity'] == 0:
            print("Edge is full")
            return float('inf')
        result += G[u][v]['intensity'] / (G[u][v]['capacity'] - G[u][v]['intensity'])
    result /= N
    print(f"Mean time delay: {result}")
    return result

# try to reroute flow in every direction
def reroute_flow(G, u, v, overflow) -> bool:
    residual = nx.subgraph_view(G, filter_edge=lambda a, b: G.has_edge(a, b) and G[a][b]['capacity'] - G[a][b]['intensity'] > 0)
    while overflow > 0:
        # filter residual capacity
        print(f"overflow: {overflow}")
        try:
            path = nx.shortest_path(residual, source=u, target=v)
            print(f"Path found: {path}")
        except nx.NetworkXNoPath:
            print(f"No path found from {u} to {v}")
            return False
        
        min_residual = min( # min cap on the path
            G[a][b]['capacity'] - G[a][b]['intensity']
            for a, b in zip(path, path[1:])
        )
        flow = min(min_residual, overflow)
        for a, b in zip(path, path[1:]):
            G[a][b]['intensity'] += flow # affects original graph
        overflow -= flow
    return True