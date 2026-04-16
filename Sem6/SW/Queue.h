#ifndef Queue_h
#define Queue_h

template <typename T>
class Queue {
private:
    struct Node {
        T element;
        Node* next = nullptr;
    };

public:
    void push(const T& element) {
        Node* n = new Node{element, nullptr};
        if (tail) {
            tail->next = n;
        } else {
            head = n;
        }
        tail = n;
    }
    T top() {
        if (head) {
            return head->element;
        }
        return nullptr;
    }
    void pop() {
        if (!head) return;

        Node* n = head;
        head = head->next;
        delete n;
    }
    bool empty() {
        return head == nullptr;
    }

private:
    Node* head = nullptr;
    Node* tail = nullptr;
};

#endif