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
            System.err.println("dupa1");
            writer.write(Integer.toString(count) + "\n");
            System.err.println("dupa2");
            writer.flush();
            System.err.println("dupa3");
            graph = mapper.readValue(reader.readLine(), Graph.class);
            System.err.println("dupa4");
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
        return graph;
    }
}
