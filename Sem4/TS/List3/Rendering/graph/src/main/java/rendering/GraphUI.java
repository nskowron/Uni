package rendering;

import java.util.ArrayList;
import java.util.List;

import javafx.scene.Group;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Line;

public class GraphUI extends Group {
    public List<Circle> nodes = new ArrayList<>();
    public List<Line> edges = new ArrayList<>();
    public List<Line> tempEdges = new ArrayList<>();

    public GraphUI() {}

    public GraphUI(Graph graph) {
        this.update(graph);
    }

    public void update(Graph graph) {
        List<Circle> newNodes = new ArrayList<>();
        for(Node node : graph.nodes) {
            newNodes.add(new Circle(node.x, node.y, 10));
        }
        nodes.addAll(newNodes);
        this.getChildren().addAll(newNodes);

        List<Line> newEdges = new ArrayList<>();
        for(Edge edge : graph.edges) {
            double from_x = nodes.get(edge.from).getCenterX();
            double from_y = nodes.get(edge.from).getCenterY();
            double to_x = nodes.get(edge.to).getCenterX();
            double to_y = nodes.get(edge.to).getCenterY();
            newEdges.add(new Line(from_x, from_y, to_x, to_y));
        }
        edges.addAll(newEdges);
        this.getChildren().addAll(newEdges);

        this.getChildren().removeAll(tempEdges);
        for(Edge edge : graph.tempEdges) {
            double from_x = nodes.get(edge.from).getCenterX();
            double from_y = nodes.get(edge.from).getCenterY();
            double to_x = nodes.get(edge.to).getCenterX();
            double to_y = nodes.get(edge.to).getCenterY();
            tempEdges.add(new Line(from_x, from_y, to_x, to_y));
        }
        this.getChildren().addAll(tempEdges);
    }
}
