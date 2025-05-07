package rendering;

import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Line;
import javafx.scene.text.Text;

public class MatrixNode extends StackPane {
    private Line edge;
    private int value;

    public MatrixNode(Line newEdge, int newValue) {
        value = newValue;
        this.getChildren().add(new Text(Integer.toString(value)));
        this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));

        if(newEdge == null) {
            this.setOnMouseDragEntered(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.RED, null, null)));
            });
            this.setOnMouseDragExited(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
            });
        }
        else {
            edge = newEdge;
            this.setOnDragEntered(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.GREEN, null, null)));
                edge.setStroke(Color.GREEN);
            });
            this.setOnDragExited(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
                edge.setStroke(Color.BLACK);
            });
            this.setOnScroll(event -> {
                if(event.getDeltaY() > 0) {
                    value += 1;
                } else {
                    value = Math.max(0, value - 1);
                }
                this.getChildren().removeFirst();
                this.getChildren().add(new Text(Integer.toString(value)));
            });

            edge.setOnDragEntered(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.GREEN, null, null)));
                edge.setStroke(Color.GREEN);
            });
            edge.setOnDragExited(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
                edge.setStroke(Color.BLACK);
            });
        }
    }
}
