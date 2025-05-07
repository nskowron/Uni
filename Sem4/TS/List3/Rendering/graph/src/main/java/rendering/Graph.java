package rendering;

import java.util.ArrayList;
import java.util.List;

public class Graph {
    public List<Node> nodes = new ArrayList<>();
    public List<Edge> edges = new ArrayList<>();
    public List<Edge> tempEdges = new ArrayList<>();

    public Graph() {}

    public void update(Graph graph) {
        nodes.addAll(graph.nodes);
        edges.addAll(graph.edges);
        tempEdges = graph.tempEdges;
    }

    public void mirrorEdges() {
        List<Edge> mirroredEdges = new ArrayList<>();
        for(Edge edge : edges) {
            mirroredEdges.add(new Edge(edge.to, edge.from, edge.weight));
        }
        edges.addAll(mirroredEdges);

        List<Edge> mirroredTempEdges = new ArrayList<>();
        for(Edge edge : tempEdges) {
            mirroredTempEdges.add(new Edge(edge.to, edge.from, edge.weight));
        }
        tempEdges.addAll(mirroredTempEdges);
    }
}

class Node {
    public int id;
    public int x;
    public int y;

    public Node() {}
    public Node(int _id, int _x, int _y) {
        id = _id;
        x = _x;
        y = _y;
    }
}

class Edge {
    public int from;
    public int to;
    public double weight;

    public Edge() {}
    public Edge(int _from, int _to, double _weight) {
        from = _from;
        to = _to;
        weight = _weight;
    }
}
