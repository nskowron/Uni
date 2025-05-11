import networkx as nx
import numpy as np

def simulate(capacities: np.ndarray, intensities: np.ndarray) -> float:
    return 33.0 # todo



def residual_subgraph(G: nx.DiGraph) -> nx.DiGraph:
    # Returns a read-only view of G with only usable edges
    return nx.subgraph_view(
        G,
        filter_edge=lambda u, v: G[u][v]['capacity'] - G[u][v].get('intensity', 0) > 0
    )


def reroute_overflow(G: nx.DiGraph, u: str, v: str) -> bool:
    edge = G[u][v]
    I = edge['intensity']
    C = edge['capacity']

    if I <= C:
        return True  # no reroute needed

    overflow = I - C
    edge['intensity'] = C  # allow up to capacity

    G_residual = residual_subgraph(G)

    try:
        path = nx.shortest_path(G_residual, source=u, target=v)
    except nx.NetworkXNoPath:
        return False

    min_residual = min(
        G[a][b]['capacity'] - G[a][b].get('intensity', 0)
        for a, b in zip(path, path[1:])
    )

    if min_residual < overflow:
        return False

    for a, b in zip(path, path[1:]):
        G[a][b]['intensity'] += overflow

    return True

