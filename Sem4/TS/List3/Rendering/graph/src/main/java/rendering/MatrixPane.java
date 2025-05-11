package rendering;

import java.util.HashMap;
import java.util.Map;

import javafx.scene.layout.GridPane;
import javafx.util.Pair;

public class MatrixPane extends GridPane {
    public static int MATRIX_SIZE_PX = 200;
    public static int MATRIX_MAX_SIZE_NODES = 500;

    public MatrixPane(Graph graph, GraphUI graphUI, WeightUpdater weight) {
        this.setMinSize(MATRIX_SIZE_PX, MATRIX_SIZE_PX);
        this.setMaxSize(MATRIX_SIZE_PX, MATRIX_SIZE_PX);
        this.setGridLinesVisible(true);

        this.add(new MatrixCell(), 0, 0);

        if(graph.nodes.size() <= MATRIX_MAX_SIZE_NODES) {
            for(int i = 0; i < graph.nodes.size(); i++) {
                this.add(new MatrixNodeCell(graph.nodes.get(i), graphUI.nodes.get(i)), 0, i + 1);
                this.add(new MatrixNodeCell(graph.nodes.get(i), graphUI.nodes.get(i)), i + 1, 0);
            }

            Map<Pair<Integer, Integer>, Boolean> actualEdges = new HashMap<>();
            for(int i = 0; i < graphUI.edges.size(); i++) {
                Edge edge1 = graph.edges.get(i);
                Edge edge2 = graph.edges.get(i + graphUI.edges.size());
                this.add(new MatrixEdgeCell(edge1.from, edge1.to, graph, graphUI, edge1, graphUI.edges.get(i), weight), edge1.from + 1, edge1.to + 1);
                this.add(new MatrixEdgeCell(edge2.from, edge2.to, graph, graphUI, edge2, graphUI.edges.get(i), weight), edge2.from + 1, edge2.to + 1);
                actualEdges.put(new Pair<>(edge1.from, edge1.to), true);
                actualEdges.put(new Pair<>(edge2.from, edge2.to), true);
            }
            for(int i = 0; i < graph.nodes.size(); i++) {
                for(int j = 0; j < graph.nodes.size(); j++) {
                    if(!actualEdges.containsKey(new Pair<>(i, j))) {
                        this.add(new MatrixEdgeCell(i, j, graph, graphUI, null, null, weight), i + 1, j + 1);
                    }
                }
            }
        }
    }
}
