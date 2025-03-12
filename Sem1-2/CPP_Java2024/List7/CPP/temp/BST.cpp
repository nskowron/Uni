#include <BST.hpp>

#include <iostream>
#include <vector>
#include <string>

template <typename T>
BST<T>::~BST()
{
    destructRec(root);
}

template <typename T>
void BST<T>::destructRec(Node<T>* node)
{
    if(node == nullptr)
    {
        return;
    }

    destructRec(node->left);
    destructRec(node->right);
    delete node;
}

template <typename T>
void BST<T>::insertRec(Node<T>* node, const T& value, const std::string& rep)
{
    if(value < node->value)
    {
        if(node->left == nullptr)
        {
            node->left = new Node<T>(value, rep);
        }
        else
        {
            insertRec(node->left, value, rep);
        }
    }
    else if(value > node->value)
    {
        if(node->right == nullptr)
        {
            node->right = new Node<T>(value, rep);
        }
        else
        {
            insertRec(node->right, value, rep);
        }
    }
}

template <typename T>
typename BST<T>::template Node<T>* BST<T>::removeRec(Node<T>* node, const T& value)
{
    if(node == nullptr)
    {
        return nullptr;
    }

    if(value < node->value)
    {
        node->left = removeRec(node->left, value);
    }
    else if(value > node->value)
    {
        node->right = removeRec(node->right, value);
    }
    else
    {
        if(node->left == nullptr && node->right == nullptr)
        {
            delete node;
            node = nullptr;
        }
        else if (node->left == nullptr)
        {
            Node<T>* temp = node;
            node = node->right;
            delete temp;
        }
        else if(node->right == nullptr)
        {
            Node<T>* temp = node;
            node = node->left;
            delete temp;
        }
        else
        {
            Node<T>* min_bigger = node->right;
            while(min_bigger->left != nullptr)
            {
                min_bigger = min_bigger->left;
            }

            node->value = min_bigger->value;
            node->value_representation = min_bigger->value_representation;
            node->right = removeRec(node->right, min_bigger->value);
        }
    }

    return node;
}

template <typename T>
bool BST<T>::searchRec(Node<T>* node, const T& value) const
{
    if(node == nullptr)
    {
        return false;
    }

    if(value < node->value)
    {
        return searchRec(node->left, value);
    }
    else if (value > node->value)
    {
        return searchRec(node->right, value);
    }
    else
    {
        return true;
    }
}

template <typename T>
void BST<T>::printRec(Node<T>* node, std::string prefix, bool isTail, std::vector<std::string>& lines) const
{
    if(node == nullptr)
    {
        return;
    }

    lines.push_back(prefix + (isTail ? "L-- " : "|-- ") + node->value_representation);

    std::vector<Node<T>*> children;
    if(node->left != nullptr)
    {
        children.push_back(node->left);
    }
    if(node->right != nullptr)
    {
        children.push_back(node->right);
    }

    for(size_t i = 0; i + 1 < children.size(); ++i)
    {
        printRec(children.at(i), prefix + (isTail ? "    " : "|   "), false, lines);
    }
    if(!children.empty())
    {
        printRec(children.at(children.size() - 1), prefix + (isTail ? "    " : "|   "), true, lines);
    }
}

template <typename T>
void BST<T>::insert(const T& value, const std::string& rep)
{
    if(root == nullptr)
    {
        root = new Node<T>(value, rep);
    }
    else
    {
        insertRec(root, value, rep);
    }
}

template <typename T>
void BST<T>::remove(const T& value)
{
    root = removeRec(root, value);
}

template <typename T>
bool BST<T>::search(const T& value) const
{
    return searchRec(root, value);
}

template <typename T>
std::vector<std::string> BST<T>::print() const
{
    std::vector<std::string> lines;
    printRec(root, "", true, lines);
    return lines;
}
