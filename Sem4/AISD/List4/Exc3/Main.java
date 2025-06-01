import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Tree tree = new RBT();

        Scanner scanner = new Scanner(System.in);

        while(true) {
            System.out.print("(i, d, p): ");
            String line = scanner.nextLine();
            String[] parts = line.split("\\s+");
            if(parts[0].equals("i")) {
                Counter.reset();
                try {
                    int value = Integer.parseInt(parts[1]);
                    tree.insert(value);
                } catch (NumberFormatException e) {}
                System.out.println("Comparisons: " + Counter.comparisons());
                System.out.println("Pointer accesses: " + Counter.pointerAccesses());
                System.out.println("Height: " + tree.height());
            } else if(parts[0].equals("d")) {
                Counter.reset();
                try {
                    int value = Integer.parseInt(parts[1]);
                    tree.delete(value);
                } catch (NumberFormatException e) {}
                System.out.println("Comparisons: " + Counter.comparisons());
                System.out.println("Pointer accesses: " + Counter.pointerAccesses());
                System.out.println("Height: " + tree.height());
            } else if(parts[0].equals("p")) {
                tree.print();
            } else {
                scanner.close();
                break;
            }
        }
    }
}