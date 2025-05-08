package rendering;

import java.util.ArrayList;
import java.util.List;

import javafx.scene.Node;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Text;

public abstract class MatrixPane extends GridPane {
    public static int MATRIX_SIZE_PX = 200;

    protected List< List<Node> > matrix = new ArrayList<>();

    public MatrixPane() {
        this.setMinSize(MATRIX_SIZE_PX, MATRIX_SIZE_PX);
        this.setMaxSize(MATRIX_SIZE_PX, MATRIX_SIZE_PX);
        
        this.setGridLinesVisible(true);

        this.set(new Text(""), 0, 0);
    }

    public abstract void update(Graph graph, GraphUI graphUI);

    public void set(Node cell, int row, int col) {
        int resize = Math.max(row, col) + 1;
        for(int i = matrix.size(); i < resize; i++) {
            matrix.add(new ArrayList<>());
        }
        for(int j = 0; j < matrix.size(); j++) {
            List<Node> rowList = matrix.get(j);
            for(int i = rowList.size(); i < resize; i++) {
                MatrixCell newCell = new MatrixCell(null, 0);
                this.add(newCell, j, i);
                rowList.add(newCell);
            }
        }

        this.getChildren().remove(matrix.get(row).get(col));
        this.add(cell, col, row);
        matrix.get(row).set(col, cell);
    }

    public MatrixCell get(int i, int j) {
        Node node = matrix.get(i).get(j);
        if(node instanceof MatrixCell) {
            return (MatrixCell)node;
        } else {
            return null;
        }
    }
}
