#include <random>
#include <iostream>
#include <array>
#include <algorithm>

int main(void) {
    std::array<int, 16> arr = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0};

    std::random_device rd;
    std::mt19937 g(rd());

    std::shuffle(arr.begin(), arr.end(), g);

    for(int i = 0; i < 16; ++i) {
        std::cout << arr[i] << ' ';
    }
    std::cout << std::endl; // flush
}