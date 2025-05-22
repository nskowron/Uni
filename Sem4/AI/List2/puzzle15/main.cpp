#include <iostream>
#include <array>
#include <chrono>

#include "check_solvable.hpp"
#include "node.hpp"
#include "solve.hpp"

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
    unsigned long long time;

    auto start_time = std::chrono::high_resolution_clock::now();
    show_path(solve(convert(arr), length, visited));

    time = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - start_time).count();

    std::cout << "length: " << length << std::endl;
    std::cout << "visited: " << visited << std::endl;
    std::cout << "time: " << time << std::endl;
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