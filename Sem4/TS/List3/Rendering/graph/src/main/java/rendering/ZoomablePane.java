package rendering;

import javafx.scene.Group;
import javafx.scene.input.ScrollEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;

public class ZoomablePane extends Pane {

    private final Group contentGroup = new Group();  // Holds your graph nodes
    private double scale = 1.0;

    // For dragging
    private double lastMouseX;
    private double lastMouseY;

    public ZoomablePane() {
        setPrefSize(800, 600);  // You can change this

        getChildren().add(contentGroup);

        // Center the group initially
        contentGroup.setTranslateX(getPrefWidth() / 2);
        contentGroup.setTranslateY(getPrefHeight() / 2);

        // Zoom handling
        this.addEventFilter(ScrollEvent.SCROLL, event -> {
            double zoomFactor = 1.05;
            if (event.getDeltaY() < 0) { // Scroll down -> zoom out
                zoomFactor = 1 / zoomFactor;
            }
            scale *= zoomFactor;

            contentGroup.setScaleX(scale);
            contentGroup.setScaleY(scale);

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

            contentGroup.setTranslateX(contentGroup.getTranslateX() + deltaX);
            contentGroup.setTranslateY(contentGroup.getTranslateY() + deltaY);

            lastMouseX = event.getSceneX();
            lastMouseY = event.getSceneY();
        });
    }

    // You can use this to add nodes to your graph
    public Group getContentGroup() {
        return contentGroup;
    }
}
