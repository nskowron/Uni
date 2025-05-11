package rendering;

import java.util.ArrayList;
import java.util.List;

public class Graph {
    public List<Node> nodes = new ArrayList<>();
    public List<Edge> edges = new ArrayList<>();
    public List<Edge> tempEdges = new ArrayList<>();

    public Graph() {}

    // Apply haskell graph update (completed)
    public void update(Graph graph) {
        nodes.addAll(graph.nodes);
        edges.addAll(graph.edges);
        tempEdges = graph.tempEdges;
    }

    /*// Mirror edges and randomise intensities
    public Graph complete(Random rng) {
        List<Edge> mirrorEdges = new ArrayList<>();
        for(Edge edge : edges) {
            Edge mirrorEdge = new Edge();
            mirrorEdge.from = edge.to;
            mirrorEdge.to = edge.from;
            mirrorEdge.weight = edge.weight;

            mirrorEdges.add(mirrorEdge);
        }
        edges.addAll(mirrorEdges);
        mirrorEdges.clear();
        for(Edge edge : tempEdges) {
            Edge mirrorEdge = new Edge();
            mirrorEdge.from = edge.to;
            mirrorEdge.to = edge.from;
            mirrorEdge.weight = edge.weight;

            mirrorEdges.add(mirrorEdge);
        }
        tempEdges.addAll(mirrorEdges);
        return this;
    }

    // Prepare python simulation
    public SimulationGraph toSimulationGraph() {
        SimulationGraph simulationGraph = new SimulationGraph();
        simulationGraph.nodes = nodes;
        simulationGraph.edges = edges;
        simulationGraph.edges.addAll(tempEdges);
        return simulationGraph;
    }*/
}

class Node {
    public int id;
    public int x;
    public int y;
}

class Edge {
    public int from;
    public int to;
    public int weight;
}
