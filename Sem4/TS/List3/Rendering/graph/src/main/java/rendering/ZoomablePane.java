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
            if(event.getDeltaY() > 0) { // scroll in
                scale *= 1.05;
            } else {                    // scroll out
                scale *= 0.95;
            }
            contentGroup.setScaleX(scale);
            contentGroup.setScaleY(scale);

            pause.playFromStart();
            pause.setOnFinished(e -> { // when user stops scrolling
                this.updateGraph(reader.read((int)(20.0 / scale))); // todo
            });
        });

        // // Dragging (panning) handling
        // this.addEventFilter(MouseEvent.MOUSE_PRESSED, event -> {
        //     lastMouseX = event.getSceneX();
        //     lastMouseY = event.getSceneY();
        // });

        // this.addEventFilter(MouseEvent.MOUSE_DRAGGED, event -> {
        //     double deltaX = event.getSceneX() - lastMouseX;
        //     double deltaY = event.getSceneY() - lastMouseY;

        //     contentGroup.setTranslateX(contentGroup.getTranslateX() + deltaX);
        //     contentGroup.setTranslateY(contentGroup.getTranslateY() + deltaY);

        //     lastMouseX = event.getSceneX();
        //     lastMouseY = event.getSceneY();
        // });
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
    }
}
