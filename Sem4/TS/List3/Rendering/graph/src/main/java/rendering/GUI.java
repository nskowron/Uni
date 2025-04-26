package rendering;

import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

public class GUI {
    public GUI(Stage stage) {
        BorderPane root = new BorderPane();
        ZoomablePane graphPane = new ZoomablePane();

        root.setCenter(graphPane);

        Scene scene = new Scene(root);
        stage.setScene(scene);
        stage.setTitle("Network Graph");
        stage.show();
    }
}
