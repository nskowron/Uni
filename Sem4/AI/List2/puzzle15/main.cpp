#include <iostream>
#include <array>

#include "check_solvable.hpp"
#include "node.hpp"
#include "solve.hpp"

extern const uint64_t TARGET_STATE;

void show_path(Node* path, int& length);

int main(void) {
    std::array<int, 16> arr;

    for(int i = 0; i < 16; ++i) {
        std::cin >> arr[i];
    }

    if(!check_solvable(arr)) { // ok
        std::cout << -1 << std::endl;
        return 0;
    }

    int length = 0;
    show_path(solve(convert(arr)), length);
    std::cout << "length: " << length << std::endl;
}

void show_path(Node* path, int& length) {
    if(path == nullptr) {
        return;
    }

    ++length;
    show_path(path->from, length);

    std::cout << path->state << std::endl;
    for(int i = 0; i < 4; ++i) {
        for(int j = 0; j < 4; ++j) {
            std::cout << (int)((path->state >> ((3 - i) * 16 + (3 - j) * 4)) & 0xFULL) << ' ';
        }
        std::cout << std::endl;
    }
    std::cout << "======================\n";
}