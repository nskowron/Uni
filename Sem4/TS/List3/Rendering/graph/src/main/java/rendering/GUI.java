package rendering;

import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

public class GUI {
    public GUI(Stage stage, GraphReader reader) {
        BorderPane root = new BorderPane();

        Graph graph = reader.read(20);
        GraphUI graphUI = new GraphUI(graph);

        ZoomablePane zoom = new ZoomablePane(graph, graphUI, reader);
        CalculationPane matrixes = new CalculationPane();
        CalculationButton experimentButton = new CalculationButton(graph, graphUI, matrixes, zoom, root);

        root.setCenter(zoom);
        root.setTop(experimentButton);

        Scene scene = new Scene(root);
        stage.setScene(scene);
        stage.setTitle("Network Graph");
        stage.show();
    }
}
