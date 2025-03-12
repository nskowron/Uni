import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.stage.Stage;
import javafx.scene.layout.VBox;
import javafx.scene.control.ScrollPane;
import java.util.logging.Level;
import javafx.scene.Scene;
import javafx.scene.layout.Priority;
import javafx.scene.control.TextField;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;

public class PascalAppGUI
{
    public PascalAppGUI(Stage stage, int limit)
    {
        ComboBox<Integer> box = new ComboBox<Integer>();
        for(int i = 0; i <= limit; ++i)
        {
            box.getItems().add(i);
        }
        box.setPromptText("Choose number of row");

        TextField textField = new TextField();
        textField.setPromptText("Choose elements");

        Label label = new Label();

        Button button = new ButtonPascalCPP("Run", box, textField, label, "./PascalCPP/main");

        BorderPane dropdown = new DropdownTextPane(box, textField, button);

        ScrollPane scroll = new ScrollPane(label);

        VBox root = new VBox();
        root.getChildren().add(dropdown);
        root.getChildren().add(scroll);

        VBox.setVgrow(scroll, Priority.ALWAYS);

        Scene scene = new Scene(root);
        stage.setScene(scene);
        stage.setMinWidth(400);
        stage.setMinHeight(300);
        stage.setWidth(400);
        stage.setHeight(300);
        stage.setTitle("Pascal's Triangle's Row");
        stage.show();
        AppLogger.logger.log(Level.INFO, "Scene has been initiated");
    }
}
