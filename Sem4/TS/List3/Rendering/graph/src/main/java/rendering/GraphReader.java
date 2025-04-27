package rendering;

import java.io.IOException;

import com.fasterxml.jackson.databind.ObjectMapper;

public class GraphReader {
    private ObjectMapper mapper = new ObjectMapper();

    public Graph read(int count) {
        Graph graph = null;
        try {
            System.out.println(count);
            System.out.flush();
            graph = mapper.readValue(System.in, Graph.class);
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
        return graph;
    }
}
