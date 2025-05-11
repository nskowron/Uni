package rendering;

public interface WeightUpdater {
    public void set(int value, Edge edge);
    public int get(Edge edge);
}
