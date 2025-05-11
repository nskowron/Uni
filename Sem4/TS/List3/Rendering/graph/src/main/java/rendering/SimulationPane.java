package rendering;

import java.util.Random;

import javafx.scene.control.Button;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Text;

public class SimulationPane extends GridPane {
    public static int SIMULATION_MAX_NODES = 500;

    public MatrixPane capacities;
    public MatrixPane intensities;
    public Text reliability;

    public SimulationPane(Graph graph, GraphUI graphUI, SimulationReader simReader) {
        this.setGridLinesVisible(true);

        this.reliability = new Text("Not Calculated");
        this.capacities = new MatrixCapacitiesPane(graph, graphUI);
        this.intensities = new MatrixIntensitiesPane(graph, graphUI, new Random());
        
        this.add(new Text("Reliability:"), 0, 0);
        this.add(reliability, 1, 0);
        this.add(new Button("Calculate") {{
            setOnAction(e -> {
                reliability.setText(simReader.read(capacities.getMatrix(), intensities.getMatrix()));
            });
        }}, 2, 0);
        this.add(new Text("Capacities Matrix"), 0, 1, 1, 3);
        this.add(capacities, 0, 2, 1, 3);
        this.add(new Text("Intensities Matrix"), 0, 3, 1, 3);
        this.add(intensities, 0, 4, 1, 3);
    }
}
