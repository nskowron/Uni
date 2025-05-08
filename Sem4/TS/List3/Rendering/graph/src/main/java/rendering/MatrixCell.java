package rendering;

import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Shape;
import javafx.scene.text.Text;

public class MatrixCell extends StackPane {
    public final Shape node;
    public int value;

    public MatrixCell(Shape newNode, int newValue) {
        node = newNode;
        value = newValue;

        this.getChildren().add(new Text(Integer.toString(value)));
        this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));

        if(node == null) {
            this.setOnMouseDragEntered(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.RED, null, null)));
            });
            this.setOnMouseDragExited(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
            });
        }
        else {
            this.setOnMouseEntered(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.GREEN, null, null)));
                node.setStroke(Color.GREEN);
                node.setFill(Color.GREEN);
            });
            this.setOnMouseExited(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
                node.setStroke(Color.BLACK);
                node.setFill(Color.BLACK);
            });
            this.setOnScroll(event -> {
                    value = Math.max(0, value + (int)event.getDeltaY() / 40);
                this.getChildren().removeFirst();
                this.getChildren().add(new Text(Integer.toString(value)));
            });

            node.setOnMouseEntered(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.GREEN, null, null)));
                node.setStroke(Color.GREEN);
                node.setFill(Color.GREEN);
            });
            node.setOnMouseExited(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
                node.setStroke(Color.BLACK);
                node.setFill(Color.BLACK);
            });
        }
    }
}
