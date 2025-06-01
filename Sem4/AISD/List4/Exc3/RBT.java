import java.util.ArrayList;
import java.util.List;

public class RBT implements Tree {
    Node root;

    @Override
    public void insert(int n) {
        Node newNode = new Node(n);
        if (root == null) {
            newNode.color = false;
            root = newNode;
        } else {
            Node temp = search(n);
            Counter.compare(temp.value, n);
            if (temp.value == n)
                return;
 
            newNode.parent = temp;

            if (Counter.compare(n, temp.value))
                temp.left = newNode;
            else
                temp.right = newNode;
            
            fixRedRed(newNode);
        }
    }
    
    @Override
    public void delete(int n) {
        if (root == null)
            return;

        Node v = search(n);

        Counter.compare(v.value, n);
        if (v.value != n) {
            return;
        }

        deleteNode(v);
    }

    @Override
    public int height() {
        return heightRec(root);
    }

    private int heightRec(Node node) {
        if(node == null) return -1;
        return 1 + Math.max(heightRec(node.left), heightRec(node.right));
    }
    
    private void leftRotate(Node x) {
        Node y = Counter.access(x.right);
        
        if (x == root)
            root = y;
            
        x.moveDown(y);
        x.right = y.left;
        if (y.left != null)
            Counter.access(y.left).parent = x;
        
        y.left = x;
    }

    private void rightRotate(Node x) {
        Node y = Counter.access(x.left);
        
        if (x == root)
            root = y;

        x.moveDown(y);
        x.left = y.right;
        if (y.right != null)
            Counter.access(y.right).parent = x;
        
        y.right = x;
    }

    private void swapColors(Node x1, Node x2) {
        boolean temp = x1.color;
        x1.color = x2.color;
        x2.color = temp;
    }

    private void swapValues(Node u, Node v) {
        int temp = u.value;
        u.value = v.value;
        v.value = temp;
    }
    
    private void fixRedRed(Node x) {
        if (x == root) {
            x.color = false;
            return;
        }
        
        // initialize parent, grandparent, uncle
        Node parent = Counter.access(x.parent), grandparent = Counter.access(Counter.access(x.parent).parent), uncle = x.uncle();

        if (parent.color) {
            if (uncle != null && uncle.color) {
                // uncle red, perform recoloring and recurse
                parent.color = false;
                uncle.color = false;
                grandparent.color = true;
                fixRedRed(grandparent);
            } else {
                // Else perform LR, LL, RL, RR
                if (parent.isOnLeft()) {
                    if (x.isOnLeft())
                        swapColors(parent, grandparent);
                    else {
                        leftRotate(parent);
                        swapColors(x, grandparent);
                    }
                    rightRotate(grandparent);
                } else {
                    if (x.isOnLeft()) {
                        rightRotate(parent);
                        swapColors(x, grandparent);
                    } else
                        swapColors(parent, grandparent);
                        
                    leftRotate(grandparent);
                }
            }
        }
    }

    private Node successor(Node x) {
        Node temp = Counter.access(x.right);
        while (temp.left != null)
            temp = Counter.access(temp.left);
        return temp;
    }
    
    private Node BSTreplace(Node x) {
        // when node have 2 children
        if (x.left != null && x.right != null)
            return successor(x);
            
        // when leaf
        if (x.left == null && x.right == null)
            return null;
            
        // when single child
        if (x.left != null)
            return Counter.access(x.left);
        else
            return Counter.access(x.right);
    }
    
    private void deleteNode(Node v) {
        Node u = BSTreplace(v);
        boolean uvBlack = ((u == null || u.color == false) && (v.color == false)); // both black
        Node parent = Counter.access(v.parent);

        if (u == null) {
            // u is NULL therefore v is leaf
            if (v == root)
                root = null;
            else {
                if (uvBlack)
                    fixDoubleBlack(v);
                    
                else if (v.sibling() != null)
                    v.sibling().color = true;
                
                if (v.isOnLeft())
                    parent.left = null;
                else
                    parent.right = null;
            }
            return;
        }

        if (v.left == null || v.right == null) {
            // v has 1 child
            if (v == root) {
                v.value = u.value;
                v.left = v.right = null;
            } else {
                if (v.isOnLeft())
                    parent.left = u;
                else
                    parent.right = u;

                u.parent = parent;

                if (uvBlack)
                    fixDoubleBlack(u);
                else
                    u.color = false;
            }
            return;
        }
        
        // v has 2 children, swap values with successor and recurse
        swapValues(u, v);
        deleteNode(u);
    }

    private void fixDoubleBlack(Node x) {
        if (x == root)
            return;

        Node sibling = x.sibling(), parent = Counter.access(x.parent);

        if (sibling == null)
        // No sibling, double black pushed up
            fixDoubleBlack(parent);
        else {
            if (sibling.color == true) {
                // sibling red
                parent.color = true;
                sibling.color = false;

                if (sibling.isOnLeft())
                    rightRotate(parent);
                else
                    leftRotate(parent);

                fixDoubleBlack(x);
            } else {
                // Sibling black
                if (sibling.hasRedChild()) {
                    // at least 1 red children
                    if (sibling.left != null && sibling.left.color == true) {
                        if (sibling.isOnLeft()) {
                            sibling.left.color = sibling.color;
                            sibling.color = parent.color;
                            rightRotate(parent);
                        } else {
                            sibling.left.color = parent.color;
                            rightRotate(sibling);
                            leftRotate(parent);
                        }
                    } else {
                        if (sibling.isOnLeft()) {
                            sibling.right.color = parent.color;
                            leftRotate(sibling);
                            rightRotate(parent);
                        } else {
                            sibling.right.color = sibling.color;
                            sibling.color = parent.color;
                            leftRotate(parent);
                        }
                    }
                    parent.color = false;
                } else {
                    // 2 black children
                    sibling.color = true;
                    if (parent.color == false)
                        fixDoubleBlack(parent);
                    else
                        parent.color = false;
                }
            }
        }
    }
    
    private Node search(int n) {
        Node temp = root;
        while (temp != null) {
            if (Counter.compare(n, temp.value)) {
                if (temp.left == null)
                    break;
                else
                    temp = temp.left;
            } else if (Counter.compare(temp.value, n)) {
                if (temp.right == null)
                    break;
                else
                    temp = temp.right;
            } else {
                break;
            }
        }

        return temp;
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
            lines.add(prefix + branchColor + (isTail ? "└── " : "├── ") + "\u001B[0m" + (node.color ? "\u001B[91m" + node.value + "\u001B[0m" : "\u001B[90m" + node.value + "\u001B[0m"));
        } else {
            String branchColor = isLeft ? "\u001B[92m" : "\u001B[91m";
            lines.add(prefix + branchColor + (isTail ? "   " : "   ") + "\u001B[0m" + (node.color ? "\u001B[91m" + node.value + "\u001B[0m" : "\u001B[90m" + node.value + "\u001B[0m"));
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

    private static class Node {
        int value;
        boolean color;
        Node left, right, parent;

        Node(int val) {
            this.value = val;
            parent = left = right = null;
            color = true;
        }
        
        Node uncle() {
            if (parent == null || Counter.access(parent).parent == null)
                return null;
            
            if (Counter.access(parent).isOnLeft()) // uncle on right
                return Counter.access(Counter.access(parent).parent).right;
            else // uncle on left
                return Counter.access(Counter.access(parent).parent).left;
        }
        
        boolean isOnLeft() {
            return this == parent.left;
        }
        
        Node sibling() {
            if (parent == null)
                return null;

            if (isOnLeft())
                return Counter.access(parent).right;

            return Counter.access(parent).left;
        }
        
        void moveDown(Node newParent) {
            if (parent != null) {
                if (isOnLeft())
                    Counter.access(parent).left = newParent;
                else
                    Counter.access(parent).right = newParent;
            }
            newParent.parent = parent;
            parent = newParent;
        }

        boolean hasRedChild() {
            return (left != null && left.color) ||
                    (right != null && right.color);
        }
    }
}