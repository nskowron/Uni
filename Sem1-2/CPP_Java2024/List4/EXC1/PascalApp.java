import javafx.application.Application;
import javafx.stage.Stage;


public class PascalApp extends Application
{
    public static void main(String[] args)
    {   
        AppLogger.Config();
        Application.launch(args);
    }

    @Override
    public void start(Stage stage)
    {   
        new PascalAppGUI(stage, 33);
    }
}
