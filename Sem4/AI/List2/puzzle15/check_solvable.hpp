#ifndef CHECK_SOLVABLE_HPP
#define CHECK_SOLVABLE_HPP

#include <array>

bool check_solvable(std::array<int, 16> arr) {
    int inversions = 0;
    int blank_row;
    for(int i = 0; i < 16; ++i) {
        if(arr[i] == 0) {
            blank_row = 3 - i / 4; // 3- -> from the bottom
            continue;
        }
        for(int j = i + 1; j < 16; ++j) {
            if(arr[j] != 0 && arr[i] > arr[j]) {
                ++inversions;
            }
        }
    }
    return (inversions + blank_row) % 2 == 0;
}

#endif