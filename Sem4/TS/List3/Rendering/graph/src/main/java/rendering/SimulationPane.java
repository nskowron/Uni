package rendering;

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
        this.capacities = new MatrixPane(graph, graphUI, new WeightUpdater() {
            @Override
            public void set(int value, Edge edge) {
                if(edge == null) {
                    return;
                }
                //edge.capacity = value;
            }
            @Override
            public int get(Edge edge) {
                if(edge == null) {
                    return 0;
                }
                return edge.weight;
            }
        });
        this.intensities = new MatrixPane(graph, graphUI, new WeightUpdater() {
            @Override
            public void set(int value, Edge edge) {
                if(edge == null) {
                    return;
                }
                //edge.intensity = value;
            }
            @Override
            public int get(Edge edge) {
                if(edge == null) {
                    return 0;
                }
                return edge.weight; //
            }
        });
        
        this.add(new Text("Reliability:"), 0, 0);
        this.add(reliability, 1, 0);
        this.add(new Button("Calculate") {{
            setOnAction(e -> {
                reliability.setText(simReader.read(graph.toSimulationGraph()));
            });
        }}, 2, 0);
        this.add(new Text("Capacities Matrix"), 0, 1, 1, 3);
        this.add(capacities, 0, 2, 1, 3);
        this.add(new Text("Intensities Matrix"), 0, 3, 1, 3);
        this.add(intensities, 0, 4, 1, 3);
    }
}
