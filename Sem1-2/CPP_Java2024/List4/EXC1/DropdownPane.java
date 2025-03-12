import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Button;
import javafx.scene.layout.Priority;
import javafx.scene.layout.ColumnConstraints;

import java.util.logging.Level;


public class DropdownPane extends BorderPane
{
    public DropdownPane(ComboBox<Integer> box, Button button)
    {
        GridPane grid = new GridPane();

        grid.add(box, 0, 0);
        grid.add(button, 1, 0);

        ColumnConstraints boxColumn = new ColumnConstraints();
        boxColumn.setHgrow(Priority.ALWAYS); 

        ColumnConstraints buttonColumn = new ColumnConstraints();
        buttonColumn.setHgrow(Priority.NEVER);
        buttonColumn.setMinWidth(button.getWidth());

        grid.getColumnConstraints().addAll(boxColumn, buttonColumn);

        this.setCenter(grid);

        AppLogger.logger.log(Level.INFO, "DropdownPane has been initiated");
    }
}
