import java.util.ArrayList;
import java.util.List;

public class BST<T extends Comparable<T>>
{
    Node<T> root;

    public BST()
    {
        this.root = null;
    };

    public class Node<E extends Comparable<E>>
    {
        E value;
        Node<E> left;
        Node<E> right;

        public Node(E value)
        {
            this.value = value;
        }
    }

    public void insert(T value)
    {
        if(root == null)
        {
            root = new Node<T>(value);
        }
        else
        {
            insertRec(root, value);
        }
    }

    public void delete(T value)
    {
        root = deleteRec(root, value);
    }

    public boolean search(T value)
    {
        return searchRec(root, value);
    }

    public List<String> print()
    {
        List<String> lines = new ArrayList<>();
        printRec(root, "", true, lines);
        return lines;
    }

    private void insertRec(Node<T> node, T value)
    {
        if(value.compareTo(node.value) < 0)
        {
            if(node.left == null)
            {
                node.left = new Node<T>(value);
            }
            else
            {
                insertRec(node.left, value);
            }
        }
        else if(value.compareTo(node.value) > 0)
        {
            if(node.right == null)
            {
                node.right = new Node<T>(value);
            }
            else
            {
                insertRec(node.right, value);
            }
        }
    }

    private Node<T> deleteRec(Node<T> node, T value)
    {
        if(node == null)
        {
            return null;
        }

        if(value.compareTo(node.value) < 0)
        {
            node.left = deleteRec(node.left, value);
        }
        else if(value.compareTo(node.value) > 0)
        {
            node.right = deleteRec(node.right, value);
        }
        else
        {
            if(node.left == null && node.right == null)
            {
                node = null;
            }
            else if(node.left == null)
            {
                node = node.right;
            }
            else if(node.right == null)
            {
                node = node.left;
            }
            else
            {
                Node<T> min_bigger = node.right;
                while(min_bigger.left != null)
                {
                    min_bigger = min_bigger.left;
                }

                node.value = min_bigger.value;
                node.right = deleteRec(node.right, min_bigger.value);
            }
        }

        return node;
    }

    private boolean searchRec(Node<T> node, T value)
    {
        if (node == null)
        {
            return false;
        }

        if(value.compareTo(node.value) < 0)
        {
            return searchRec(node.left, value);
        }
        else if(value.compareTo(node.value) > 0)
        {
            return searchRec(node.right, value);
        }
        else
        {
            return true;
        }
    }

    private void printRec(Node<T> node, String prefix, boolean isTail, List<String> lines)
    {
        if (node == null)
        {
            return;
        }

        lines.add(prefix + (isTail ? "└── " : "├── ") + node.value);

        List<Node<T>> children = new ArrayList<>();
        if(node.left != null)
        {
            children.add(node.left);
        }
        if(node.right != null)
        {
            children.add(node.right);
        }

        for(int i = 0; i < children.size() - 1; i++)
        {
            printRec(children.get(i), prefix + (isTail ? "    " : "│   "), false, lines);
        }
        if(!children.isEmpty())
        {
            printRec(children.get(children.size() - 1), prefix + (isTail ? "    " : "│   "), true, lines);
        }
    }
}