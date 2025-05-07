package rendering;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javafx.scene.layout.GridPane;
import javafx.scene.text.Text;
import javafx.util.Pair;

public class MatrixesPane extends GridPane {
    public MatrixPane capacities;
    public MatrixPane intensities;

    public MatrixesPane() {
        this.setGridLinesVisible(true);

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
                    this.set(new MatrixCell(graphUI.edges.get(j), (int)edge.weight / 3), edge.from + 1, edge.to + 1);
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

            @Override
            public void update(Graph graph, GraphUI graphUI) {
                // dont make a matrix over 500
                if(graph.nodes.size() > 500) {
                    this.getChildren().clear();
                    this.matrix.clear();
                    this.set(new Text("..."), 0, 0);
                    return;
                }

                int oldSize = matrix.size();

                // set nodes
                for(int i = matrix.size(); i < graph.nodes.size(); i++) {
                    this.set(new MatrixCell(graphUI.nodes.get(i), i), 0, i + 1);
                    this.set(new MatrixCell(graphUI.nodes.get(i), i), i + 1, 0);
                }

                // set edges
                for(int i = oldSize; i < graph.edges.size(); i++) {
                    for(int j = 0; j < graph.edges.size(); j++) {
                        if(i == j) {
                            continue;
                        }
                        this.set(new MatrixCell(null, rng.nextInt(6)), i + 1, j + 1);
                        this.set(new MatrixCell(null, rng.nextInt(6)), j + 1, i + 1);
                    }
                }
            }
        };

        this.add(new Text("Capacities"), 0, 0);
        this.add(capacities, 0, 1);
        this.add(new Text("Intensities"), 0, 2);
        this.add(intensities, 0, 3);
    }

    public void update(Graph graph, GraphUI graphUI) {
        capacities.update(graph, graphUI);
        intensities.update(graph, graphUI);
    }
}
