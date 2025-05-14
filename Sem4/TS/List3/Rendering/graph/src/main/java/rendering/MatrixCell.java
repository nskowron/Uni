package rendering;

import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Line;
import javafx.scene.text.Text;

public class MatrixCell extends StackPane {
    public MatrixCell() {
        this.setPrefSize(MatrixPane.MATRIX_SIZE_PX / 10, MatrixPane.MATRIX_SIZE_PX / 10);
        this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
        this.setStyle("-fx-border-color: dimgray; -fx-border-width: 1px;");

        this.setOnScroll(event -> {
            event.consume();
        });
    }
}

class MatrixNodeCell extends MatrixCell {
    public MatrixNodeCell(int id, Circle nodeUI) {
        super();

        this.getChildren().add(new Text(Integer.toString(id)));

        this.setOnMouseEntered(event -> {
            this.setBackground(new Background(new BackgroundFill(Color.GREEN, null, null)));
            nodeUI.setStroke(Color.GREEN);
            nodeUI.setFill(Color.GREEN);
        });
        this.setOnMouseExited(event -> {
            this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
            nodeUI.setStroke(Color.BLACK);
            nodeUI.setFill(Color.BLACK);
        });

        nodeUI.setOnMouseEntered(event -> {
            this.setBackground(new Background(new BackgroundFill(Color.GREEN, null, null)));
            nodeUI.setStroke(Color.GREEN);
            nodeUI.setFill(Color.GREEN);
        });
        nodeUI.setOnMouseExited(event -> {
            this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
            nodeUI.setStroke(Color.BLACK);
            nodeUI.setFill(Color.BLACK);
        });
    }
    
}

class MatrixEdgeCell extends MatrixCell {
    public int value;

    public MatrixEdgeCell(int _value, Line edgeUI, boolean diagonal) {
        super();

        if(diagonal) {
            value = 0;
        } else {
            value = _value;
        }

        this.getChildren().add(new Text(Integer.toString(value)));
        this.setStyle("-fx-border-color: lightgray;");

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
            if(!diagonal) {
                value = Math.max(0, value + (int)event.getDeltaY() / 4);
                this.getChildren().removeFirst();
                this.getChildren().add(new Text(Integer.toString(value)));
            }
            event.consume();
        });

        if(edgeUI != null) {
            edgeUI.setOnMouseEntered(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.GREEN, null, null)));
                edgeUI.setStroke(Color.GREEN);
            });
            edgeUI.setOnMouseExited(event -> {
                this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
                edgeUI.setStroke(Color.BLACK);
            });
        }
    }
}
