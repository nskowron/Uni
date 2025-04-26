package rendering;

import java.io.IOException;

import com.fasterxml.jackson.databind.ObjectMapper;

public class GraphReader {
    private ObjectMapper mapper = new ObjectMapper();

    public Graph read() {
        Graph graph = null;
        try {
            graph = mapper.readValue(System.in, Graph.class);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return graph;
    }
}
