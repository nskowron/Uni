#include <iostream>
#include <array>

#include "check_solvable.hpp"
#include "node.hpp"
#include "solve.hpp"

extern const uint64_t TARGET_STATE;

void show_path(Node* path);

int main(void) {
    std::array<int, 16> arr;

    for(int i = 0; i < 16; ++i) {
        std::cin >> arr[i];
    }

    if(!check_solvable(arr)) { // ok
        std::cout << -1 << std::endl;
        return 0;
    }

    int length, visited;
    show_path(solve(convert(arr), length, visited));
    std::cout << "length: " << length << std::endl;
    std::cout << "visited: " << visited << std::endl;
}

void show_path(Node* path) {
    if(path == nullptr) {
        return;
    }

    show_path(path->from);

    std::cout << "Step: " << (int)path->g() << "\n";
    std::cout << "Heury: " << (int)path->h() << "\n";
    std::cout << path->state << std::endl;
    for(int i = 0; i < 4; ++i) {
        for(int j = 0; j < 4; ++j) {
            std::cout << (int)((path->state >> ((3 - i) * 16 + (3 - j) * 4)) & 0xFULL) << ' ';
        }
        std::cout << "\n";
    }
    std::cout << "======================\n";

    delete path;
}