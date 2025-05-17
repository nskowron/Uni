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

Node* solve(uint64_t start) {
    std::priority_queue<std::unique_ptr<Node>, std::vector< std::unique_ptr<Node> >, Node::Compare> to_search;
    std::unordered_map<uint64_t, std::unique_ptr<Node>> searched_path;

    to_search.emplace(std::make_unique<Node>(start, 0, manhattan(start), nullptr));
    Node* result = nullptr;

    while(!to_search.empty()) {
        Node* current = const_cast<std::unique_ptr<Node>&>(to_search.top()).release(); // top -> O(1)
        to_search.pop();

        // exit condition sort of
        if(searched_path.find(TARGET_STATE) != searched_path.end() && // find -> O(1)
           searched_path.find(TARGET_STATE)->second->g() < current->f()) // we are only left with bigger paths than already found
        {
            result = searched_path.find(TARGET_STATE)->second.release();
            Node* from = result->from;
            while(from != nullptr) {
                searched_path.find(from->state)->second.release(); // allow to go out of scope
                from = from->from;
            }
            break;
        }

        // check if we've been here through a shorter path
        if(searched_path.find(current->state) != searched_path.end() &&
           searched_path.find(current->state)->second->g() <= current->g())
        {
            continue; // dont search neighbors
        }

        // mark new path and keep searching
        searched_path[current->state] = std::unique_ptr<Node>(current);
        uint64_t neighbors[4];
        int c = get_neighbors(current->state, neighbors);
        while(c-- > 0) {
            to_search.emplace(std::make_unique<Node>(neighbors[c], current->g() + 1, manhattan(neighbors[c]), current));
        }
    }

    return result;
}

#endif