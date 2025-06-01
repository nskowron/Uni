import java.util.ArrayList;
import java.util.List;

public class Splay implements Tree {
    private Node root;

    private static class Node {
        int value; 
        Node left, right;

        Node(int val) {
            value = val; 
            left = right = null;
        }
    };

    @Override
    public void insert(int key) {
        if (root == null) root = new Node(key);;

        root = splay(root, key); 

        Counter.compare(root.value, key);
        if (root.value == key) return; 

        Node newNode = new Node(key); 

        if (Counter.compare(key, root.value)) { 
            newNode.right = root; 
            newNode.left = Counter.access(root.left); 
            root.left = null; 
            root = newNode;
        } else { 
            newNode.left = root; 
            newNode.right = Counter.access(root.right); 
            root.right = null; 
            root = newNode;
        }
    }

    @Override
    public void delete(int key) {
        if (root == null) return; 

        root = splay(root, key); 

        Counter.compare(root.value, key);
        if (root.value != key) return; 

        if (root.left == null) {
            root = Counter.access(root.right); 
        } else {
            Node temp = root;
            root = splay(Counter.access(root.left), key); 
            root.right = Counter.access(temp.right);
        }
    }

    @Override
    public int height() {
        return heightRec(root);
    }

    private int heightRec(Node node) {
        if (node == null) return -1; 
        return 1 + Math.max(heightRec(node.left), heightRec(node.right));
    }

    private Node rightRotate(Node x) { 
        Node y = x.left; 
        x.left = y.right; 
        y.right = x; 
        return y; 
    }

    private Node leftRotate(Node x) { 
        Node y = x.right; 
        x.right = y.left; 
        y.left = x; 
        return y; 
    }

    private Node splay(Node root, int value) {
        if (root == null || root.value == value) 
            return root; 

        if (Counter.compare(value, root.value)) {
            if (root.left == null) return root; 

            if (Counter.compare(value, Counter.access(root.left).value)) {
                Counter.access(root.left).left = splay(Counter.access(Counter.access(root.left).left), value); 
                root = rightRotate(root); 
            } else if (Counter.compare(Counter.access(root.left).value, value)) {
                Counter.access(root.left).right = splay(Counter.access(Counter.access(root.left).right), value); 
                if (Counter.access(root.left).right != null) 
                    root.left = leftRotate(Counter.access(root.left)); 
            }

            return (root.left == null) ? root : rightRotate(root); 
        } else {
            if (root.right == null) return root; 

            if (Counter.compare(value, Counter.access(root.right).value)) {
                Counter.access(root.right).left = splay(Counter.access(Counter.access(root.right).left), value); 
                if (Counter.access(root.right).left != null) 
                    root.right = rightRotate(Counter.access(root.right)); 
            } else if (Counter.compare(Counter.access(root.right).value, value)){
                Counter.access(root.right).right = splay(Counter.access(Counter.access(root.right).right), value); 
                root = leftRotate(root); 
            } 

            return (root.right == null) ? root : leftRotate(root); 
        }
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
