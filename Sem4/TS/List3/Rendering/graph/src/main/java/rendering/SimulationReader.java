package rendering;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class SimulationReader {
    private final DataInputStream reader;
    private final DataOutputStream writer;

    public SimulationReader(InputStream in, OutputStream out) {
        reader = new DataInputStream(in);
        writer = new DataOutputStream(out);
    }

    public double read(int[][] capacities, int[][] intensities) {
        double result = -1;
        try {
            writer.writeInt(capacities.length);
            for(int[] cap : capacities) {
                for(int c : cap) {
                    writer.writeInt(c);
                }
            }
            for(int[] intes : intensities) {
                for(int i : intes) {
                    writer.writeInt(i);
                }
            }
            writer.flush();
            result = reader.readDouble();
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
        return result;
    }
}
