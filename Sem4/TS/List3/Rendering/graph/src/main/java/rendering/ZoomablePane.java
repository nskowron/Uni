package rendering;

import java.util.ArrayList;
import java.util.List;

import javafx.animation.PauseTransition;
import javafx.scene.Group;
import javafx.scene.input.ScrollEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Line;
import javafx.util.Duration;

public class ZoomablePane extends Pane {
    private final Group contentGroup = new Group();
    private Graph graph;
    private int nodes = 20;

    // For scrolling
    private double scale = 1.0;
    private final PauseTransition pause = new PauseTransition(Duration.millis(200));

    // For dragging
    private double lastMouseX;
    private double lastMouseY;

    public ZoomablePane(GraphReader reader) {
        setPrefSize(800, 600);
        getChildren().add(contentGroup);

        // Center the group initially
        contentGroup.setTranslateX(getPrefWidth() / 2);
        contentGroup.setTranslateY(getPrefHeight() / 2);

        // Initial graph
        this.updateGraph(reader.read(20));

        // Zoom handling
        this.setOnScroll(event -> {
            double prevScale = scale;
            scale *= Math.exp(event.getDeltaY() * 0.0015);

            double dx = event.getSceneX() - (this.getBoundsInParent().getMinX() + contentGroup.getTranslateX() + this.getTranslateX());
            double dy = event.getSceneY() - (this.getBoundsInParent().getMinY() + contentGroup.getTranslateY() + this.getTranslateY());
            double f = (scale / prevScale) - 1;

            this.setScaleX(scale);
            this.setScaleY(scale);
            this.setTranslateX(this.getTranslateX() + f * dx);
            this.setTranslateY(this.getTranslateY() + f * dy);

            pause.playFromStart();
            pause.setOnFinished(e -> { // when user stops scrolling
                int nodesVisible = (int)(60.0 / scale - 40); // todo
                if(nodesVisible > graph.nodes.length) {
                    this.updateGraph(reader.read(nodesVisible));
                }
            });

            event.consume();
        });

        // Dragging (panning) handling
        this.addEventFilter(MouseEvent.MOUSE_PRESSED, event -> {
            lastMouseX = event.getSceneX();
            lastMouseY = event.getSceneY();
        });

        this.addEventFilter(MouseEvent.MOUSE_DRAGGED, event -> {
            double deltaX = event.getSceneX() - lastMouseX;
            double deltaY = event.getSceneY() - lastMouseY;

            this.setTranslateX(this.getTranslateX() + deltaX);
            this.setTranslateY(this.getTranslateY() + deltaY);

            lastMouseX = event.getSceneX();
            lastMouseY = event.getSceneY();
        });
    }

    private void updateGraph(Graph newGraph) {
        if(newGraph == null) {
            System.err.println("whoopsie daisy happenned");
            return;
        }

        contentGroup.getChildren().clear();
        graph = newGraph;

        List<Circle> nodes = new ArrayList<>();
        List<Line> edges = new ArrayList<>();
        for(Node node : graph.nodes) {
            nodes.add(new Circle(node.x, node.y, 10));
        }
        for(Edge edge : graph.edges) {
            edges.add(new Line(
                nodes.get(edge.from).getCenterX(),
                nodes.get(edge.from).getCenterY(),
                nodes.get(edge.to).getCenterX(),
                nodes.get(edge.to).getCenterY()
            ));
        }

        contentGroup.getChildren().addAll(nodes);
        contentGroup.getChildren().addAll(edges);

        contentGroup.getChildren().add(new Circle(0, 0, 50)); // debug
    }
}
