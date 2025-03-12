import java.util.List;

import javafx.application.Platform;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;

public class Cell extends Thread
{
    private final Rectangle guiRect;

    private static Randomiser random = new Randomiser();

    private Color color;
    private final Object lock;
    private List<Cell> neighbors;
    private final long sleepTime;
    private final double probability;

    private boolean active;
    private boolean exists;

    public Cell(Object lock, long sleepTime, double probability)
    {
        color = random.nextColor();

        guiRect = new Rectangle(30, 30, color);
        guiRect.setOnMouseClicked((me)->
        {
            toggleActive();
        });

        neighbors = null;
        this.lock = lock;
        this.sleepTime = sleepTime;
        this.probability = probability;

        active = true;
        exists = false;
    }

    public void setup(List<Cell> neighbors) throws IllegalArgumentException
    {
        if(neighbors.size() != 4)
        {
            throw new IllegalArgumentException("Each Cell needs to have 4 neighbors, got: " + neighbors.size());
        }

        this.neighbors = neighbors;
        exists = true;
        this.start();
    }

    public Rectangle getGuiRectangle()
    {
        return guiRect;
    }

    public synchronized boolean isExisting()
    {
        return exists;
    }

    public synchronized void destroy()
    {
        exists = false;
    }

    public synchronized boolean isActive()
    {
        return active;
    }

    public synchronized void toggleActive()
    {
        active = !active;
    }

    public synchronized Color getColor()
    {
        return color;
    }

    public void updateColor()
    {
        synchronized(lock)
        {
            ThreadLogger.logStart(this);

            synchronized(this)
            {
                if(!isActive())
                {
                    return;
                }

                if(random.nextDouble(100.0) <= probability)
                {
                    color = random.nextColor();
                }
                else
                {
                    synchronized(neighbors.get(0))
                    {
                        synchronized(neighbors.get(1))
                        {
                            synchronized(neighbors.get(2))
                            {
                                synchronized(neighbors.get(3))
                                {
                                    int activeNeighbors = 0;
                                    double red = 0;
                                    double green = 0;
                                    double blue = 0;

                                    for(Cell neighbor : neighbors)
                                    {
                                        if(neighbor.isActive())
                                        {
                                            activeNeighbors++;
                                            Color nColor = neighbor.getColor();
                                            red += nColor.getRed();
                                            green += nColor.getGreen();
                                            blue += nColor.getBlue();
                                        }
                                    }

                                    if(activeNeighbors != 0)
                                    {
                                        color = new Color(red / activeNeighbors, green / activeNeighbors, blue / activeNeighbors, 1.0);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Platform.runLater(()->
            {
                guiRect.setFill(color);
            });

            ThreadLogger.logEnd(this);
        }
    }

    @Override
    public void run()
    {
        while(isExisting())
        {
            updateColor();

            try
            {
                Thread.sleep((long)(sleepTime * random.nextDouble(0.5, 1.5)));
            }
            catch(InterruptedException e)
            {
                ErrorHandler.showError("Thread error", "Thread nr " + this.threadId() + " has been interrupted.");
                destroy();
            }
        }
    }
}