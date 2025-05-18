#ifndef SOLVE_HPP
#define SOLVE_HPP

#include <cstdint>
#include <queue>
#include <unordered_map>
#include <memory>

#include <iostream> //tmp

#include "node.hpp"
#include "state.hpp"
#include "heuristics.hpp"

extern const uint64_t TARGET_STATE;

Node* solve(uint64_t start, int& length, int& visited) {
    WD_load(); //

    std::priority_queue<std::unique_ptr<Node>, std::vector< std::unique_ptr<Node> >, Node::Compare> to_search;
    std::unordered_map<uint64_t, std::unique_ptr<Node>> searched_path;

    // to_search.emplace(std::make_unique<Node>(start, 0, manhattan_linear(start), nullptr)); //
    to_search.emplace(std::make_unique<Node>(start, 0, walking_distance(start), nullptr)); //
    Node* result = nullptr;

    while(!to_search.empty()) {
        Node* current = const_cast<std::unique_ptr<Node>&>(to_search.top()).release(); // top -> O(1)
        to_search.pop(); // pop -> O(1)

        // exit condition - ok because monotonic
        if(current->state == TARGET_STATE) {
            result = current;
            current = current->from;
            while(current != nullptr) {
                searched_path.find(current->state)->second.release(); // allow to go out of scope
                current = current->from;
            }
            break;
        }

        // check if we've been here
        if(searched_path.find(current->state) != searched_path.end()) { // find -> avg O(1)
            delete current;
            continue; // dont search neighbors
        }

        // mark new path and keep searching
        searched_path[current->state] = std::unique_ptr<Node>(current); // avg O(1)

        uint64_t neighbors[4];
        int c = get_neighbors(current->state, neighbors);
        while(c-- > 0) {
            if(searched_path.find(neighbors[c]) == searched_path.end()) { // not visited
                // to_search.emplace(std::make_unique<Node>(neighbors[c], current->g() + 1, manhattan_linear(neighbors[c]), current)); // emplace -> O(log n)
                to_search.emplace(std::make_unique<Node>(neighbors[c], current->g() + 1, walking_distance(neighbors[c]), current)); //
            }
        }
    }

    visited = searched_path.size();
    length = result == nullptr ? -1 : result->g();
    return result;
}

#endif