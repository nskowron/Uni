import java.io.IOException;
import java.util.logging.*;

public class AppLogger
{
    private AppLogger() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of a static class AppLogger");
    }

    public static final Logger logger = Logger.getGlobal();

    public static void Config()
    {
        for(Handler handler : logger.getHandlers())
        {
            logger.removeHandler(handler);
        }
        logger.setUseParentHandlers(false);

        ConsoleHandler ch = new ConsoleHandler();
        ch.setLevel(Level.INFO);
        ch.setFormatter(new SimpleFormatter());
        logger.addHandler(ch);

        try
        {
            FileHandler fh = new FileHandler("./log.txt");
            fh.setLevel(Level.ALL);
            fh.setFormatter(new SimpleFormatter());
            logger.addHandler(fh);
        }
        catch (IOException | SecurityException e)
        {
            logger.log(Level.SEVERE, "Error while creating FileHandler", e);
        }

        logger.setLevel(Level.ALL);
    }
}
