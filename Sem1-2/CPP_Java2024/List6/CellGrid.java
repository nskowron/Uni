import java.util.ArrayList;
import java.util.List;

import javafx.scene.layout.GridPane;

public class CellGrid
{
    private final GridPane guiGrid;

    private final List<Cell> cells;

    private final Object lock = new Object();

    private int width;
    private int height;

    public CellGrid(int width, int height, long sleepTime, double probability) throws IllegalArgumentException
    {
        if(width < 0 || height < 0)
        {
            throw new IllegalArgumentException("CellGrid size cannot be negative, got: " + width + " x " + height);
        }

        this.width = width;
        this.height = height;

        guiGrid = new GridPane();
        cells = new ArrayList<>(0);

        for(int x = 0; x < width; ++x)
        {
            for(int y = 0; y < height; ++y)
            {
                addCell(new Cell(lock, sleepTime, probability), x, y);
            }
        }

        for(int x = 0; x < width; ++x)
        {
            for(int y = 0; y < height; ++y)
            {
                List<Cell> neighbors = new ArrayList<>(0);
                neighbors.add(get(x-1, y));
                neighbors.add(get(x, y+1));
                neighbors.add(get(x+1, y));
                neighbors.add(get(x, y-1));
                get(x, y).setup(neighbors);
            }
        }
    }

    public GridPane getGuiGrid()
    {
        return guiGrid;
    }

    public void addCell(Cell cell, int x, int y)
    {
        guiGrid.add(cell.getGuiRectangle(), x, y);
        cells.addLast(cell);
    }

    public Cell get(int x, int y)
    {
        x = x % width;
        y = y % height;

        if(x < 0)
        {
            x += width;
        }
        if(y < 0)
        {
            y += height;
        }

        return cells.get(x * height + y);
    }

    public void destroy()
    {
        for(int x = 0; x < width; ++x)
        {
            for(int y = 0; y < height; ++y)
            {
                get(x, y).destroy();
            }
        }
    }
}
