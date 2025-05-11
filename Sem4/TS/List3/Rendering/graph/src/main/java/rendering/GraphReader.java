package rendering;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import com.fasterxml.jackson.databind.ObjectMapper;

public class GraphReader {
    private final ObjectMapper mapper = new ObjectMapper();
    private final BufferedReader reader;
    private final BufferedWriter writer;

    public GraphReader(InputStream in, OutputStream out) {
        reader = new BufferedReader(new InputStreamReader(in));
        writer = new BufferedWriter(new OutputStreamWriter(out));
    }

    public Graph read(int count) {
        Graph graph = null;
        try {
            writer.write(Integer.toString(count) + "\n");
            writer.flush();
            graph = mapper.readValue(reader.readLine(), Graph.class);
        } catch (IOException e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
        return graph;
    }
}
