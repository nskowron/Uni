package rendering;

import javafx.scene.control.Button;
import javafx.scene.layout.BorderPane;

public class CalculationButton extends Button {
    private boolean experimentMode = false;

    public CalculationButton(Graph graph, GraphUI graphUI, CalculationPane matrixes, ZoomablePane zoom, BorderPane root) {
        super("Calculate");
        setOnAction(e -> {
            if(experimentMode == false) {
                matrixes.update(graph, graphUI);
                root.setRight(matrixes);
                zoom.experimentMode = true;
                experimentMode = true;
                this.setText("Back");
            }
            else {
                root.setRight(null);
                root.setBottom(null);
                zoom.experimentMode = false;
                experimentMode = false;
                this.setText("Calculate");
            }
        });
    }
}
