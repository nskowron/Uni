package rendering;

import javafx.scene.layout.GridPane;

public class MatrixPane extends GridPane {
    public static int MATRIX_SIZE = 200;

    public MatrixPane() {
        this.setPrefSize(MATRIX_SIZE, MATRIX_SIZE);

        //matrix = new Label[graph.nodes.length][graph.nodes.length]; //labels?
    }
}
