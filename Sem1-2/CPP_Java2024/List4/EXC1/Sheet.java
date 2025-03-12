import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.Node;

import java.util.logging.Level;

public class Sheet extends GridPane
{
    public Sheet(int x, int y) throws IllegalArgumentException
    {
        if(x < 0 || y < 0)
        {
            throw new IllegalArgumentException("Sheet cannot have negative number of columns / rows, got: " + x + " / " + y);
        }

        for(int j = 0; j < y; ++j)
        {
            for(int i = 0; i < x; ++i)
            {
                Label label = new Label();
                add(label, i, j);
            }
        }

        AppLogger.logger.log(Level.INFO, "Sheet " + x + "x" + y + " has been initiated");
    }

    public Label Get(int x, int y) throws IndexOutOfBoundsException
    {
        return (Label) getChildren().get(y * getColumnCount() + x);
    }

    public void Clear()
    {
        for(Node node : getChildren())
        {
            ((Label)node).setText("");
        }
    }
}
