import networkx as nx

G = nx.DiGraph()
G.add_edge(0, 1, capacity=10)
G.add_edge(1, 2, capacity=5)
G.add_edge(2, 3, capacity=15)
G.add_edge(3, 4, capacity=10)

H = nx.subgraph_view(G, filter_edge=lambda u, v: G.has_edge(u, v) and G[u][v]['capacity'] < 15)

path = []
try:
    path = nx.shortest_path(H, source=0, target=4)
except nx.NetworkXNoPath:
    print("No path found")

for u, v in zip(path, path[1:]):
    print(f"{u} -> {v}")

print(f"{G[2][3]['capacity']}")