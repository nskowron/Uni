package rendering;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import com.fasterxml.jackson.databind.ObjectMapper;

public class GraphReader {
    private ObjectMapper mapper = new ObjectMapper();
    private BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

    public Graph read(int count) {
        Graph graph = null;
        try {
            System.out.println(count);
            System.out.flush();
            graph = mapper.readValue(reader.readLine(), Graph.class);
            graph.mirrorEdges(); // to reduce json size
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
        return graph;
    }
}
