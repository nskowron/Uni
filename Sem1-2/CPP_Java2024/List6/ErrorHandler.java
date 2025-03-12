import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;

/**
 * Utility class for displaying error messages.
 */
public class ErrorHandler {

    // Private constructor to prevent instantiation
    private ErrorHandler() {
        throw new InstantiationError("Cannot create instance of static class ErrorHandler");
    }

    /**
     * Displays an error message in an alert dialog.
     * @param title The title of the error dialog.
     * @param message The error message to display.
     * @brief Displays an error message in an alert dialog.
     */
    public static void showError(String title, String message) {
        // Create a new alert dialog with error type
        Alert alert = new Alert(AlertType.ERROR);
        
        // Set the title and content of the alert
        alert.setTitle(title);
        alert.setHeaderText(null); // No header text
        alert.setContentText(message);
        
        // Show the alert dialog and wait for user interaction
        alert.showAndWait();
    }
}
