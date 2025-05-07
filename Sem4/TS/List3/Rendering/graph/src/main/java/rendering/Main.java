package rendering;

import javafx.application.Application;
import javafx.stage.Stage;

public class Main extends Application {
    @Override
    public void start(Stage stage) {
        new GUI(stage, new GraphReader());
    }

    public static void main(String[] args) {
        launch();
    }
}