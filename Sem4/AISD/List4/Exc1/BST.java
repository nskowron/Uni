import java.util.ArrayList;
import java.util.List;

public class BST implements Tree {
    private Node root;

    private static class Node {
        int value;
        Node left, right;

        Node(int value) {
            this.value = value;
            this.left = this.right = null;
        }
    }

    @Override
    public void insert(int value) {
        root = insertRec(root, value);
    }

    private Node insertRec(Node node, int value) {
        if (node == null) {
            return new Node(value);
        }
        if (Counter.compare(value, node.value)) {
            node.left = insertRec(Counter.access(node.left), value);
        } else if (Counter.compare(node.value, value)) {
            node.right = insertRec(Counter.access(node.right), value);
        }
        return node;
    }

    @Override
    public void delete(int value) {
        root = deleteRec(root, value);
    }

    private Node deleteRec(Node node, int value) {
        if (node == null) {
            return null;
        }
        if (Counter.compare(value, node.value)) {
            node.left = deleteRec(Counter.access(node.left), value);
        } else if (Counter.compare(node.value, value)) {
            node.right = deleteRec(Counter.access(node.right), value);
        } else {
            // Node with one child or no child
            if (node.left == null) return Counter.access(node.right);
            else if (node.right == null) return Counter.access(node.left);

            // Node with two children: Get the successor
            node.value = minValue(Counter.access(node.right));

            // Delete the successor
            node.right = deleteRec(Counter.access(node.right), node.value);
        }
        return node;
    }

    private int minValue(Node node) {
        int minValue = node.value;
        while (node.left != null) {
            node = Counter.access(node.left);
            minValue = node.value;
        }
        return minValue;
    }

    @Override
    public int height() {
        return heightRec(root);
    }
    private int heightRec(Node node) {
        if (node == null) {
            return -1; // Height of an empty tree is -1
        }
        return Math.max(heightRec(node.left), heightRec(node.right)) + 1;
    }

    @Override
    public void print() {
        List<String> lines = new ArrayList<>();
        printRec(root, "", true, true, true, lines);
        for (String line : lines) {
            System.out.println(line);
        }
    }

    private void printRec(Node node, String prefix, boolean isTail, boolean isLeft, boolean isFirst, List<String> lines) {
        if (node == null) return;

        if (!isFirst) {
            String branchColor = isLeft ? "\u001B[92m" : "\u001B[91m";
            lines.add(prefix + branchColor + (isTail ? "└── " : "├── ") + "\u001B[0m" + node.value);
        } else {
            String branchColor = isLeft ? "\u001B[92m" : "\u001B[91m";
            lines.add(prefix + branchColor + (isTail ? "   " : "   ") + "\u001B[0m" + node.value);
        }
        List<Node> children = new ArrayList<>();
        List<Boolean> isLeftList = new ArrayList<>();

        if (node.left != null) {
            children.add(node.left);
            isLeftList.add(true);
        }
        if (node.right != null) {
            children.add(node.right);
            isLeftList.add(false);
        }

        for (int i = 0; i < children.size(); i++) {
            boolean childIsTail = (i == children.size() - 1);
            String newPrefix = prefix + (isTail ? "    " : "\u001B[90m" + "│   " + "\u001B[0m");
            printRec(children.get(i), newPrefix, childIsTail, isLeftList.get(i), false, lines);
        }
    }
}