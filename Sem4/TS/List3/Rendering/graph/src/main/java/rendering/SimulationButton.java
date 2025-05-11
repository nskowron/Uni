package rendering;

import javafx.scene.control.Button;
import javafx.scene.layout.BorderPane;

public class SimulationButton extends Button {
    private boolean simMode = false;

    public SimulationButton(Graph graph, GraphUI graphUI, SimulationReader simReader, ZoomablePane zoom, BorderPane root) {
        super("Simulation");
        setOnAction(e -> {
            if(simMode == false) { // Create simulation pane
                root.setRight(new SimulationPane(graph, graphUI, simReader));
                zoom.simMode = true;
                simMode = true;
                this.setText("Back");
            }
            else { // Remove simulation pane
                root.setRight(null);
                zoom.simMode = false;
                simMode = false;
                this.setText("Simulation");
            }
        });
    }
}
