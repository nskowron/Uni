package rendering;

import java.util.Random;

import javafx.animation.PauseTransition;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.util.Duration;

public class ZoomablePane extends Pane {
    private final GraphUI graphUI;
    private final Graph graph;

    public boolean simMode = false;

    // For scrolling
    private double scale = 1.0;
    private final PauseTransition pause = new PauseTransition(Duration.millis(200));

    // For dragging
    private double lastMouseX;
    private double lastMouseY;

    public ZoomablePane(Graph initialGraph, GraphUI initialGraphUI, GraphReader reader, Random rng) {
        graph = initialGraph;
        graphUI = initialGraphUI;
        
        this.setPrefSize(800, 600);
        this.getChildren().add(graphUI);

        // Initial graph
        graphUI.setTranslateX(getPrefWidth() / 2);
        graphUI.setTranslateY(getPrefHeight() / 2);

        // Zoom handling
        this.setOnScroll(event -> {
            if(simMode) {
                event.consume();
                return;
            }

            double prevScale = scale;
            scale *= Math.exp(event.getDeltaY() * 0.0015);

            double dx = event.getSceneX() - (this.getBoundsInParent().getMinX() + graphUI.getTranslateX() + this.getTranslateX());
            double dy = event.getSceneY() - (this.getBoundsInParent().getMinY() + graphUI.getTranslateY() + this.getTranslateY());
            double f = (scale / prevScale) - 1;

            this.setScaleX(scale);
            this.setScaleY(scale);
            this.setTranslateX(this.getTranslateX() + f * dx);
            this.setTranslateY(this.getTranslateY() + f * dy);

            pause.playFromStart();
            pause.setOnFinished(e -> { // when user stops scrolling
                int nodesVisible = (int)(60.0 / scale - 40); // todo
                if(nodesVisible > graph.nodes.size()) {
                    this.updateGraph(reader.read(nodesVisible), rng);
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

    public void updateGraph(Graph graphUpdate, Random rng) {
        graphUI.update(graphUpdate);
        graph.update(graphUpdate);
    }
}
