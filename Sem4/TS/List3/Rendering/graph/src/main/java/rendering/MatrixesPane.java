package rendering;

import javafx.scene.layout.GridPane;

public class MatrixesPane extends GridPane {
    public MatrixPane capacities = new MatrixPane();
    public MatrixPane intensities = new MatrixPane();

    public MatrixesPane() {
        this.setGridLinesVisible(true);

        this.add(capacities, 0, 0);
        this.add(intensities, 0, 1);
    }
}
