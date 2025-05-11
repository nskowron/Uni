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
        try (ServerSocket socket = new ServerSocket(port)){
            socket.accept(); // netcat check
            Socket gen_socket = socket.accept();
            Socket sim_socket = socket.accept();
            new GUI(
                stage,
                new GraphReader(gen_socket.getInputStream(), gen_socket.getOutputStream()),
                new SimulationReader(sim_socket.getInputStream(), sim_socket.getOutputStream())
            );
        } catch(IOException e) {
            System.err.println("Whoopse daisy! ServerrRRR");
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        launch();
    }
}