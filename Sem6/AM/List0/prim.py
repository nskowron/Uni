import networkx as nx
import heapq

def prim(G):
    mst = nx.Graph()
    in_mst = [False] * (len(G.nodes) + 1)
    in_mst[1] = True
    pq = []
    for v in G.neighbors(1):
        weight = G[1][v]['weight']
        heapq.heappush(pq, (weight, 1, v))
    while pq:
        weight, u, v = heapq.heappop(pq)
        if in_mst[v]:
            continue
        in_mst[v] = True
        mst.add_edge(u, v, weight=weight)
        for w in G.neighbors(v):
            if not in_mst[w]:
                weight = G[v][w]['weight']
                heapq.heappush(pq, (weight, v, w))
    return mst

def dfs(G, v, route):
    route.append(v)
    for u in G.neighbors(v):
        if u not in route:
            dfs(G, u, route)