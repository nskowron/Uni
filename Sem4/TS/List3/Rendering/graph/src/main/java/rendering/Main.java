package rendering;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import javafx.application.Application;
import javafx.stage.Stage;

public class Main extends Application {
    public static final int port = 2137;
    
    @Override
    public void start(Stage stage) {
        CommunicationChannel gen = null;
        CommunicationChannel sim = null;
        try (ServerSocket socket = new ServerSocket(port)){
            //System.err.println("dupa11");
            Socket gen_socket = socket.accept();
            //System.err.println("dupa22");
            gen = new CommunicationChannel(gen_socket.getInputStream(), gen_socket.getOutputStream());
            //sim = new CommunicationChannel(null, null);
            //System.err.println("dupa33");
            new GUI(stage, new GraphReader(gen), new SimulationReader(sim));
        } catch(IOException e) {
            System.err.println("Whoopse daisy! ServerrRRR");
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        launch();
    }
}