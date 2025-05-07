package rendering;

import javafx.scene.control.Button;
import javafx.scene.layout.BorderPane;

public class ExperimentButton extends Button {
    private boolean experimentMode = false;

    public ExperimentButton(Experiment experiment, Graph graph, GraphUI graphUI, MatrixesPane matrixes, ZoomablePane zoom, BorderPane root) {
        super("Experiment");
        setOnAction(e -> {
            if(experimentMode == false) {
                matrixes.update(graph, graphUI);
                root.setRight(matrixes);
                root.setBottom(new Button("Run") {{
                    setOnAction(event1 -> {
                        experiment.runExperiment();
                    });
                }});
                experimentMode = true;
                this.setText("Back");
            }
            else {
                root.setRight(null);
                root.setBottom(null);
                experimentMode = false;
                this.setText("Experiment");
            }
        });
    }
}
