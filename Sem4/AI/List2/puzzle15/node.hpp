#ifndef NODE_HPP
#define NODE_HPP

#include <cstdint>
#include <memory>

struct Node {
    uint64_t state;
    uint16_t cost; // g | f
    Node* from;

    inline uint8_t g() const {
        return (uint8_t)(cost >> 8);
    }
    inline uint8_t f() const {
        return (uint8_t)(cost & 0xFF);
    }
    inline uint8_t h() const {
        return f() -  g();
    }

    Node(uint64_t s, uint8_t g, uint8_t h, Node* f) : 
        state(s),
        cost(((uint16_t)g << 8) | (g + h)),
        from(f)
    {}

    struct Compare {
        bool operator()(const Node* a, const Node* b) const {
            return a->f() > b->f();
        }
    };
};

#endif