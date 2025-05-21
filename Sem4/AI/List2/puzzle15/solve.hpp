#ifndef SOLVE_HPP
#define SOLVE_HPP

#include <cstdint>
#include <queue>
#include <unordered_map>
#include <memory>

#include "node.hpp"
#include "state.hpp"
#include "heuristics.hpp"

extern const uint64_t TARGET_STATE;

Node* solve(uint64_t start, int& length, int& visited) {
    visited = 0;
    length = 0;

    WD_load(); // walking distance database

    Node* start_node = new Node(start, 0, heuristics(start), nullptr);
    if(start == TARGET_STATE) {
        return start_node;
    }

    std::priority_queue<Node*, std::vector<Node*>, Node::Compare> to_search;
    std::unordered_map<uint64_t, std::unique_ptr<Node>> searched_path;

    to_search.emplace(start_node);
    searched_path[start] = std::unique_ptr<Node>(start_node);

    while(!to_search.empty()) {
        Node* current = to_search.top(); // top -> O(1)
        to_search.pop(); // pop -> O(1)

        uint64_t neighbors[4];
        uint8_t c = get_neighbors(current->state, neighbors);
        while(c-- > 0) {
            if(searched_path.find(neighbors[c]) == searched_path.end()) { // not visited 
                Node* neighbor_node = new Node(neighbors[c], current->g() + 1, heuristics(neighbors[c]), current);
                if(neighbors[c] == TARGET_STATE) { // exit condition - ok because monotonic
                    visited = searched_path.size();
                    length = neighbor_node->g();
                    while(current != nullptr) {
                        searched_path.find(current->state)->second.release(); // allow to go out of scope
                        current = current->from;
                    }
                    return neighbor_node;
                }
                searched_path[neighbors[c]] = std::unique_ptr<Node>(neighbor_node);
                to_search.emplace(neighbor_node); // emplace -> O(log n)
            }
        }
    }

    return nullptr;
}

#endif