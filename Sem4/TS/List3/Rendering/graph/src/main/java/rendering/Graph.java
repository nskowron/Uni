package rendering;

public class Graph {
    public Node[] nodes;
    public Edge[] edges;
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
