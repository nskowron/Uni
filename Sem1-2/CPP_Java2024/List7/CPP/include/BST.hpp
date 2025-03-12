#ifndef BST_HPP
#define BST_HPP

#include <vector>
#include <string>

template <typename T>
class BST
{
private:
    template <typename E>
    class Node
    {
    public:
        E value;
        std::string value_representation;
        Node<E>* left;
        Node<E>* right;

        Node(E val, std::string rep) : value(val), value_representation(rep), left(nullptr), right(nullptr) {}
    };

    Node<T>* root;

public:
    BST() : root(nullptr) {}
    ~BST();

    void destructRec(Node<T>*);

    void insert(const T&, const std::string&);
    void remove(const T&);
    bool search(const T&) const;
    std::vector<std::string> print() const;

private:
    void insertRec(Node<T>*, const T&, const std::string&);
    Node<T>* removeRec(Node<T>*, const T&);
    bool searchRec(Node<T>*, const T&) const;
    void printRec(Node<T>*, std::string, bool, std::vector<std::string>&) const;
};

#include <BST.cpp>

#endif