package rendering;

import java.util.ArrayList;
import java.util.List;

public class Matrix {
    private List< List<Double> > data;

    public Matrix(int n, List<Edge> edges) {
        data = new ArrayList<>(n);
        for(List<Double> col : data) {
            col = new ArrayList<>(n);
            for(int i = 0; i < n; ++i) {
                col.set(i, 0.0);
            }
        }

        for(Edge edge : edges) {
            data.get(edge.from).set(edge.to, edge.weight);
        }
    }
}
