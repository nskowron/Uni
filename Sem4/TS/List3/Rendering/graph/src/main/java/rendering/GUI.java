package rendering;

import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

public class GUI {
    public GUI(Stage stage, GraphReader reader) {
        BorderPane root = new BorderPane();

        Graph initialGraph = reader.read(20);
        GraphUI initialGraphUI = new GraphUI(initialGraph);

        ZoomablePane zoom = new ZoomablePane(initialGraph, initialGraphUI, reader);
        ExperimentPane experiment = new ExperimentPane(initialGraph, initialGraphUI);

        root.setCenter(zoom);

        Scene scene = new Scene(root);
        stage.setScene(scene);
        stage.setTitle("Network Graph");
        stage.show();
    }
}
