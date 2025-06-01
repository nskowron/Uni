public class Counter {
    private static int comp = 0;
    private static int ptr_acc = 0;

    private Counter() {}

    public static int comparisons() {
        return comp;
    }

    public static int pointerAccesses() {
        return ptr_acc;
    }

    public static void reset() {
        comp = 0;
        ptr_acc = 0;
    }

    public static boolean compare(int a, int b) {
        comp++;
        return a < b;
    }

    public static <N> N access(N node) {
        ptr_acc++;
        return node;
    }
}