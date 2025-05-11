package rendering;

import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;

public class MatrixCell extends StackPane {
    public MatrixCell() {
        this.setBackground(new Background(new BackgroundFill(Color.WHITE, null, null)));
    }
}
