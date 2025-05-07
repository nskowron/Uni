package rendering;

import javafx.scene.layout.BorderPane;

public class ExperimentPane extends BorderPane {
    private Graph graph;
    private GraphUI graphUI;

    private MatrixesPane matrixes;

    public ExperimentPane(Graph initialGraph, GraphUI initialGraphUI) {
        graph = initialGraph;
        graphUI = initialGraphUI;
    }

    public void runExperiment() {} // todo
}
