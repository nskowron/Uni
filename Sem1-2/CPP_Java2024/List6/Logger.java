import java.io.IOException;
import java.util.logging.*;

/**
 * @brief Utility class for logging messages in the Paint application.
 *
 * This class provides methods to configure logging settings and log messages to the console and a log file.
 */
public class Logger {

    /**
     * @brief Private constructor to prevent instantiation of the PaintLogger class.
     *
     * This constructor throws an InstantiationError if someone tries to create an instance of this static class.
     */
    private Logger() throws InstantiationError {
        throw new InstantiationError("Cannot create instance of a static class PaintLogger");
    }

    /** The global logger instance for the Paint application. */
    public static final java.util.logging.Logger logger = java.util.logging.Logger.getGlobal();

    /**
     * @brief Configures the logger with specified settings.
     *
     * This method configures the logger to log messages to both the console and a log file.
     * It sets log levels and adds appropriate handlers for console and file logging.
     */
    public static void Config() {
        // Remove existing handlers to avoid duplicate logging
        for (Handler handler : logger.getHandlers()) {
            logger.removeHandler(handler);
        }
        logger.setUseParentHandlers(false); // Prevent logging to the parent logger's handlers

        // Create a ConsoleHandler for logging to the console
        ConsoleHandler ch = new ConsoleHandler();
        ch.setLevel(Level.INFO); // Set log level to INFO
        ch.setFormatter(new SimpleFormatter()); // Use SimpleFormatter for log formatting
        logger.addHandler(ch); // Add the ConsoleHandler to the logger

        try {
            // Create a FileHandler for logging to a file named "log.txt" in the current directory
            FileHandler fh = new FileHandler("./log.txt");
            fh.setLevel(Level.ALL); // Set log level to ALL (logs all levels)
            fh.setFormatter(new SimpleFormatter()); // Use SimpleFormatter for log formatting
            logger.addHandler(fh); // Add the FileHandler to the logger
        } catch (IOException | SecurityException e) {
            // Log an error message if there's an exception while creating the FileHandler
            logger.log(Level.SEVERE, "Error while creating FileHandler", e);
        }

        logger.setLevel(Level.ALL); // Set the logger's log level to ALL (logs all levels)
    }
}
