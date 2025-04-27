package rendering;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Scanner;

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