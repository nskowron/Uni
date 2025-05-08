package rendering;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javafx.scene.control.Button;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Text;
import javafx.util.Pair;

public class CalculationPane extends GridPane {
    public MatrixPane capacities;
    public MatrixPane intensities;
    public Text meanTimeDelay;

    private int n = 0;

    public CalculationPane() {
        this.setGridLinesVisible(true);

        meanTimeDelay = new Text("Not Calculated");
        capacities = new MatrixPane() {
            List< Pair<Integer, Integer> > tempEdges = new ArrayList<>();

            @Override
            public void update(Graph graph, GraphUI graphUI) {
                // dont make a matrix over 500
                if(graph.nodes.size() > 500) {
                    this.getChildren().clear();
                    this.matrix.clear();
                    this.set(new Text("..."), 0, 0);
                    return;
                }

                // set nodes
                for(int i = matrix.size(); i < graph.nodes.size(); i++) {
                    this.set(new MatrixCell(graphUI.nodes.get(i), i), 0, i + 1);
                    this.set(new MatrixCell(graphUI.nodes.get(i), i), i + 1, 0);
                }

                // delete temp edges
                for(Pair<Integer, Integer> edge : tempEdges) {
                    this.set(new MatrixCell(null, 0), edge.getKey(), edge.getValue());
                }
                tempEdges.clear();

                // set edges
                int j = 0;
                while(j < graph.edges.size()) {
                    int x = graph.edges.get(j).from;
                    int y = graph.edges.get(j).to;

                    if(matrix.get(x).get(y) instanceof MatrixCell) {
                        if(((MatrixCell)matrix.get(x).get(y)).node == null) {
                            break;
                        }
                    }
                    else {
                        break;
                    }

                    j++;
                }
                while(j < graph.edges.size()) {
                    Edge edge = graph.edges.get(j);
                    this.set(new MatrixCell(graphUI.edges.get(j), (int)edge.weight / 2), edge.from + 1, edge.to + 1);
                    j++;
                }
                // set temp edges
                for(int i = 0; i < graph.tempEdges.size(); i++) {
                    Edge edge = graph.tempEdges.get(i);
                    this.set(new MatrixCell(graphUI.tempEdges.get(i), (int)edge.weight / 3), edge.from + 1, edge.to + 1);
                    tempEdges.add(new Pair<>(edge.from, edge.to));
                }
            }
        };

        intensities = new MatrixPane() {
            private Random rng = new Random();
            List< Pair<Integer, Integer> > tempEdges = new ArrayList<>();

            @Override
            public void update(Graph graph, GraphUI graphUI) {
                // dont make a matrix over 500
                if(graph.nodes.size() > 500) {
                    this.getChildren().clear();
                    this.matrix.clear();
                    this.set(new Text("..."), 0, 0);
                    return;
                }

                // set nodes
                for(int i = matrix.size(); i < graph.nodes.size(); i++) {
                    this.set(new MatrixCell(graphUI.nodes.get(i), i), 0, i + 1);
                    this.set(new MatrixCell(graphUI.nodes.get(i), i), i + 1, 0);
                }

                // delete temp edges
                for(Pair<Integer, Integer> edge : tempEdges) {
                    this.set(new MatrixCell(null, 0), edge.getKey(), edge.getValue());
                }
                tempEdges.clear();

                // set edges
                int j = 0;
                while(j < graph.edges.size()) {
                    int x = graph.edges.get(j).from;
                    int y = graph.edges.get(j).to;

                    if(matrix.get(x).get(y) instanceof MatrixCell) {
                        if(((MatrixCell)matrix.get(x).get(y)).node == null) {
                            break;
                        }
                    }
                    else {
                        break;
                    }

                    j++;
                }
                while(j < graph.edges.size()) {
                    Edge edge = graph.edges.get(j);
                    this.set(new MatrixCell(graphUI.edges.get(j), rng.nextInt(6)), edge.from + 1, edge.to + 1);
                    j++;
                }
                // set temp edges
                for(int i = 0; i < graph.tempEdges.size(); i++) {
                    Edge edge = graph.tempEdges.get(i);
                    this.set(new MatrixCell(graphUI.tempEdges.get(i), rng.nextInt(6)), edge.from + 1, edge.to + 1);
                    tempEdges.add(new Pair<>(edge.from, edge.to));
                }
            }
        };

        this.add(new Button("Recalculate") {{
            setOnAction(e -> {
                meanTimeDelay.setText(Double.toString(calculateMeanTimeDelay()));
            });
        }}, 0, 0);
        this.add(new Text("Mean Time Delay"), 0, 1);
        this.add(meanTimeDelay, 0, 2);
        this.add(new Text("Capacities"), 0, 3);
        this.add(capacities, 0, 4);
        this.add(new Text("Intensities"), 0, 5);
        this.add(intensities, 0, 6);
    }

    public void update(Graph graph, GraphUI graphUI) {
        capacities.update(graph, graphUI);
        intensities.update(graph, graphUI);
        n = graph.nodes.size();
        meanTimeDelay.setText(Double.toString(this.calculateMeanTimeDelay()));
    }

    public double calculateMeanTimeDelay() {
        if(n > 500) {
            return -1;
        }

        double delay = 0;
        for(int i = 1; i <= n; i++) {
            for(int j = 1; j <= n; j++) {
                double cap = (double)capacities.get(i, j).value;
                double inten = (double)intensities.get(i, j).value;
                if(cap - inten > 0) {
                    delay += inten / (cap - inten);
                }
            }
        }
        return delay / (double)n;
    }
}
