package rendering;

import java.io.IOException;

import com.fasterxml.jackson.databind.ObjectMapper;

public class SimulationReader {
    private final ObjectMapper mapper = new ObjectMapper();
    private final CommunicationChannel channel;

    public SimulationReader(CommunicationChannel _channel) {
        this.channel = _channel;
    }

    public String read(SimulationGraph graph) {
        String result = null;
        try {
            channel.writer.write(mapper.writeValueAsString(graph));
            channel.writer.newLine();
            channel.writer.flush();
            result = channel.reader.readLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return result;
    }
}
