import json
import networkx as nx
import matplotlib.pyplot as plt
import os
import time

def read_graph(path):
    try:
        with open(path, 'r') as f:
            return json.load(f)
    except (json.JSONDecodeError, FileNotFoundError):
        return None

def build_graph(data):
    G = nx.Graph()
    for node in data['nodes']:
        G.add_node(node['id'], pos=(node['x'], node['y']))
    for edge in data['edges']:
        G.add_edge(edge['from'], edge['to'], weight=edge['weight'])
    return G

def draw_graph(G, ax):
    ax.clear()
    pos = nx.spring_layout(G)
    nx.draw(G, pos, with_labels=True, node_color='skyblue', edge_color='gray', node_size=500, ax=ax)
    plt.show(block=True)

def main():
    plt.ion()
    _, ax = plt.subplots()

    path = 'graph_fifo'
    while True:
        data = read_graph(path)
        if data:
            G = build_graph(data)
            draw_graph(G, ax)
        else:
            time.sleep(1)

if __name__ == "__main__":
    main()