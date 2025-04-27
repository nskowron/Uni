package rendering;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import com.fasterxml.jackson.databind.ObjectMapper;

public class GraphReader {
    private ObjectMapper mapper = new ObjectMapper();
    private BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

    public Graph read() {
        Graph graph = null;
        try {
            String json = reader.readLine();
            graph = mapper.readValue(json, Graph.class);
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
        return graph;
    }

    public Graph read(int count) {
        Graph graph = null;
        try {
            System.out.println(count);
            System.out.flush();
            System.err.println("dupa1");
            String json = reader.readLine();
            graph = mapper.readValue(json, Graph.class);
            System.err.println("dupa2");
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
        return graph;
    }
}
