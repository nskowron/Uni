package rendering;

import java.io.IOException;

import com.fasterxml.jackson.databind.ObjectMapper;

public class GraphReader {
    private final ObjectMapper mapper = new ObjectMapper();
    private final CommunicationChannel channel;

    public GraphReader(CommunicationChannel _channel) {
        channel = _channel;
    }

    public Graph read(int count) {
        Graph graph = null;
        try {
            System.err.println("dupa1");
            channel.writer.write(Integer.toString(count) + "\n");
            System.err.println("dupa2");
            channel.writer.flush();
            System.err.println("dupa3");
            graph = mapper.readValue(channel.reader.readLine(), Graph.class);
            System.err.println("dupa4");
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
        return graph;
    }
}
