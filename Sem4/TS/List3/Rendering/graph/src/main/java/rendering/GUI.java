package rendering;

import java.util.Random;

import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

public class GUI {
    public GUI(Stage stage, GraphReader graphReader, SimulationReader simReader) {
        BorderPane root = new BorderPane();

        Graph graph = graphReader.read(20);
        GraphUI graphUI = new GraphUI(graph);

        ZoomablePane zoom = new ZoomablePane(graph, graphUI, graphReader, new Random());
        SimulationButton simButton = new SimulationButton(graph, graphUI, simReader, zoom, root);

        root.setCenter(zoom);
        root.setTop(simButton);

        Scene scene = new Scene(root);
        stage.setScene(scene);
        stage.setTitle("Network Graph");
        stage.show();
    }
}
