import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.stage.Stage;
import javafx.scene.layout.VBox;
import javafx.scene.control.ScrollPane;
import java.util.logging.Level;
import javafx.scene.Scene;
import javafx.scene.layout.Priority;

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

        Sheet sheet = new Sheet(2 * (limit + 1), (limit + 1));

        Button button = new ButtonPascal("Render", sheet, box);

        DropdownPane dropdown = new DropdownPane(box, button);

        ScrollPane scroll = new ScrollPane(sheet);

        VBox root = new VBox();
        root.getChildren().add(dropdown);
        root.getChildren().add(scroll);

        VBox.setVgrow(scroll, Priority.ALWAYS);

        Scene scene = new Scene(root);
        stage.setScene(scene);
        stage.setMinWidth(600);
        stage.setMinHeight(500);
        stage.setWidth(600);
        stage.setHeight(500);
        stage.setTitle("Pascal's Triangle");
        stage.show();
        AppLogger.logger.log(Level.INFO, "Scene has been initiated");
    }
}
