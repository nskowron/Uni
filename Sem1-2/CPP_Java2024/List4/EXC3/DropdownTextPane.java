import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;
import javafx.scene.control.Button;
import javafx.scene.layout.Priority;
import javafx.scene.layout.ColumnConstraints;

import java.util.logging.Level;


public class DropdownTextPane extends BorderPane
{
    public DropdownTextPane(ComboBox<Integer> box, TextField textArea, Button button)
    {
        GridPane grid = new GridPane();

        grid.add(box, 0, 0);
        grid.add(textArea, 1, 0);
        grid.add(button, 2, 0);

        ColumnConstraints boxColumn = new ColumnConstraints();
        boxColumn.setMinWidth(box.getWidth());

        ColumnConstraints textColumn = new ColumnConstraints();
        textColumn.setHgrow(Priority.ALWAYS);

        ColumnConstraints buttonColumn = new ColumnConstraints();
        buttonColumn.setHgrow(Priority.NEVER);
        buttonColumn.setMinWidth(button.getWidth());

        grid.getColumnConstraints().addAll(boxColumn, textColumn, buttonColumn);

        this.setCenter(grid);

        AppLogger.logger.log(Level.INFO, "DropdownTextPane has been initiated");
    }
}
