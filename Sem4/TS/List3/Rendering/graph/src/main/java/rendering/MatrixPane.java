package rendering;

import java.util.ArrayList;
import java.util.List;

import javafx.scene.Node;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Text;

public abstract class MatrixPane extends GridPane {
    public static int MATRIX_SIZE_PX = 200;
    //public static MatrixCell EMPTY_CELL = new MatrixCell(null, 0);

    protected List< List<Node> > matrix = new ArrayList<>();

    public MatrixPane() {
        this.setPrefSize(MATRIX_SIZE_PX, MATRIX_SIZE_PX);
        this.setGridLinesVisible(true);

        this.set(new Text(""), 0, 0);
    }

    public abstract void update(Graph graph, GraphUI graphUI);

    public void set(Node cell, int row, int col) {
        int resize = Math.max(row, col) + 1;
        for(int i = matrix.size(); i < resize; i++) {
            matrix.add(new ArrayList<>());
        }
        for(List<Node> rowList : matrix) {
            for(int i = rowList.size(); i < resize; i++) {
                rowList.add(new MatrixCell(null, 0));
            }
        }

        this.getChildren().remove(matrix.get(row).get(col));
        this.add(cell, col, row);
        matrix.get(row).set(col, cell);
    }
}
