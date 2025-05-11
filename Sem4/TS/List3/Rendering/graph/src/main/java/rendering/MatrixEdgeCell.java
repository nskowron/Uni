package rendering;

import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.paint.Color;
import javafx.scene.shape.Line;
import javafx.scene.text.Text;

public class MatrixEdgeCell extends MatrixCell {
    private int value;
    private Edge edge;
    private Line edgeUI;

    public MatrixEdgeCell(int x, int y, Graph graph, GraphUI graphUI, Edge _edge, Line _edgeUI, WeightUpdater weight) {
        super();

        edge = _edge;
        edgeUI = _edgeUI;
        value = weight.get(edge); // needs to return 0 if edge is null

        this.getChildren().add(new Text(Integer.toString(value)));

        this.setOnMouseEntered(event -> {
            if(edgeUI != null) {
                this.setBackground(new Background(new BackgroundFill(Color.GREEN, null, null)));
                edgeUI.setStroke(Color.GREEN);
            } else {
                this.setBackground(new Background(new BackgroundFill(Color.RED, null, null)));
            }
        });

        this.setOnMouseExited(event -> {
            this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
            if(edgeUI != null) {
                edgeUI.setStroke(Color.BLACK);
            }
        });

        this.setOnScroll(event -> {
            value = Math.max(0, value + (int)event.getDeltaY() / 40);
            this.getChildren().removeFirst();
            this.getChildren().add(new Text(Integer.toString(value)));
            /*if(edge == null && value > 0) { // Create new edge temporarily
                edge = new Edge();
                edge.from = x;
                edge.to = y;
                weight.set(value, edge);
                edgeUI = new Line(graphUI.nodes.get(x).getCenterX(), graphUI.nodes.get(x).getCenterY(), graphUI.nodes.get(y).getCenterX(), graphUI.nodes.get(y).getCenterY());
                graph.tempEdges.add(edge);
                graphUI.tempEdges.add(edgeUI);
                graphUI.getChildren().add(edgeUI);
            }
            if(edge != null) {
                weight.set(value, edge);
            }*/
        });
    }

    public int getValue() {
        return value;
    }
}
