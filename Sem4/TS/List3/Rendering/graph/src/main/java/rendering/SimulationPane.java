package rendering;

import java.util.Random;

import javafx.scene.control.Button;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Text;

public class SimulationPane extends GridPane {
    public static int SIMULATION_MAX_NODES = 500;

    public SimulationPane(Graph graph, GraphUI graphUI, SimulationReader simReader) {
        this.setPrefWidth(MatrixPane.MATRIX_SIZE_PX);
        this.setGridLinesVisible(true);

        Text result = new Text("Not Calculated");
        GridPane simulation = new GridPane();
        MatrixPane capacities = new MatrixCapacitiesPane(graph, graphUI);
        MatrixPane intensities = new MatrixIntensitiesPane(graph, graphUI, new Random());
        
        simulation.add(new Text("Reliability: "), 0, 0);
        simulation.add(result, 1, 0);
        simulation.add(new Button("Calculate") {{
            setOnAction(e -> {
                if(graph.nodes.size() > SIMULATION_MAX_NODES) {
                    result.setText("Graph too big:c");
                } else {
                    result.setText(Double.toString(simReader.read(capacities.getMatrix(), intensities.getMatrix())));
                }
            });
        }}, 2, 0);
        for(int i = 0; i < 3; i++) {
            ColumnConstraints cc = new ColumnConstraints();
            cc.setPercentWidth(33.3);
            simulation.getColumnConstraints().add(cc);
        }

        this.add(simulation, 0, 0);
        this.add(new Text("Capacities Matrix"), 0, 1);
        this.add(capacities, 0, 2);
        this.add(new Text("Intensities Matrix"), 0, 3);
        this.add(intensities, 0, 4);
    }
}
