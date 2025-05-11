package rendering;

import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.text.Text;

public class MatrixNodeCell extends MatrixCell {
    public final Node node;
    public final Circle nodeUI;
    public int value;

    public MatrixNodeCell(Node newNode, Circle newNodeUI) {
        super();

        node = newNode;
        nodeUI = newNodeUI;
        value = node.id;

        this.getChildren().add(new Text(Integer.toString(value)));

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
    }
    
}
