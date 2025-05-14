package rendering;

import java.util.Random;

import javafx.scene.control.ScrollPane;
import javafx.scene.layout.GridPane;
import javafx.scene.shape.Line;

public abstract class MatrixPane extends ScrollPane {
    public static int MATRIX_SIZE_PX = 200;
    public static int MATRIX_MAX_SIZE_NODES = 500;

    protected final MatrixEdgeCell[][] matrix;
    protected final GridPane grid;

    public MatrixPane(int n) {
        this.grid = new GridPane();
        if(n < MATRIX_MAX_SIZE_NODES) {
            matrix = new MatrixEdgeCell[n][n];
        } else {
            matrix = null;
        }
        this.setPrefSize(MATRIX_SIZE_PX, MATRIX_SIZE_PX);
        this.setContent(grid);
    }

    public int[][] getMatrix() {
        int[][] result = new int[matrix.length][matrix.length];
        for(int i = 0; i < matrix.length; i++) {
            for(int j = 0; j < matrix.length; j++) {
                result[i][j] = matrix[i][j].value;
            }
        }
        return result;
    }
}

class MatrixIntensitiesPane extends MatrixPane {
    public MatrixIntensitiesPane(Graph graph, GraphUI graphUI, Random rng) {
        super(graph.nodes.size());

        grid.add(new MatrixCell(), 0, 0); // corner cell

        if(graph.nodes.size() < MATRIX_MAX_SIZE_NODES) {
            for(int i = 0; i < graph.nodes.size(); i++) { // nodes
                grid.add(new MatrixNodeCell(i, graphUI.nodes.get(i)), i + 1, 0);
                grid.add(new MatrixNodeCell(i, graphUI.nodes.get(i)), 0, i + 1);
            }
            for(int i = 0; i < matrix.length; i++) { // all fields
                for(int j = 0; j < matrix.length; j++) {
                    MatrixEdgeCell cell = new MatrixEdgeCell(rng.nextInt(Math.max(200 / graph.nodes.size(), 2)), null, i == j);
                    grid.add(cell, i + 1, j + 1);
                    matrix[i][j] = cell;
                }
            }
        }
    }
}

class MatrixCapacitiesPane extends MatrixPane {
    public MatrixCapacitiesPane(Graph graph, GraphUI graphUI) {
        super(graph.nodes.size());

        grid.add(new MatrixCell(), 0, 0); // corner cell

        if(graph.nodes.size() < MATRIX_MAX_SIZE_NODES) {
            for(int i = 0; i < graph.nodes.size(); i++) { // nodes
                grid.add(new MatrixNodeCell(i, graphUI.nodes.get(i)), i + 1, 0);
                grid.add(new MatrixNodeCell(i, graphUI.nodes.get(i)), 0, i + 1);
            }
            for(int i = 0; i < graph.edges.size(); i++) { // edges
                Edge edge = graph.edges.get(i);
                Line edgeUI = graphUI.edges.get(i);
                MatrixEdgeCell cell1 = new MatrixEdgeCell(edge.weight * 5, edgeUI, edge.from == edge.to);
                MatrixEdgeCell cell2 = new MatrixEdgeCell(edge.weight * 5, edgeUI, edge.from == edge.to);
                grid.add(cell1, edge.from + 1, edge.to + 1);
                grid.add(cell2, edge.to + 1, edge.from + 1);
                matrix[edge.from][edge.to] = cell1;
                matrix[edge.to][edge.from] = cell2;
            }
            for(int i = 0; i < graph.tempEdges.size(); i++) { // tempEdges
                Edge edge = graph.tempEdges.get(i);
                Line edgeUI = graphUI.tempEdges.get(i);
                MatrixEdgeCell cell1 = new MatrixEdgeCell(edge.weight * 5, edgeUI, edge.from == edge.to);
                MatrixEdgeCell cell2 = new MatrixEdgeCell(edge.weight * 5, edgeUI, edge.from == edge.to);
                grid.add(cell1, edge.from + 1, edge.to + 1);
                grid.add(cell2, edge.to + 1, edge.from + 1);
                matrix[edge.from][edge.to] = cell1;
                matrix[edge.to][edge.from] = cell2;
            }
            for(int i = 0; i < matrix.length; i++) { // all other fields
                for(int j = 0; j < matrix.length; j++) {
                    if(matrix[i][j] == null) {
                        MatrixEdgeCell cell = new MatrixEdgeCell(0, null, i == j);
                        grid.add(cell, i + 1, j + 1);
                        matrix[i][j] = cell;
                    }
                }
            }
        }
    }
}
