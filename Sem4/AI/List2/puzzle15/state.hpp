#ifndef STATE_HPP
#define STATE_HPP

#include <cstdint>
#include <array>

#include <iostream> //tmp

const uint64_t TARGET_STATE = 0x123456789ABCDEF0;

uint64_t convert(std::array<int, 16> arr) {
    uint64_t state = 0;
    for(int i = 0; i < 16; ++i) {
        state |= (uint64_t)arr[i] << (15 - i) * 4;
    }
    return state;
}

inline uint64_t swap(uint64_t state, int z, int x) { // zero pos, x pos
    return (state | (((state >> (x * 4)) & 0xFULL) << (z * 4))) & (~(0xFULL << (x * 4)));
}

inline int get_neighbors(uint64_t state, uint64_t neighbors[4]) {
    int z = 0;
    while(((state >> (z * 4)) & 0xFULL) != 0) {
        ++z;
    }

    int c = 0;
    if(z > 3) { // non-bottom row
        neighbors[c++] = swap(state, z, z - 4);
    }
    if(z < 12) { // non-top row
        neighbors[c++] = swap(state, z, z + 4);
    }
    if(z % 4 > 0) { // non-right column
        neighbors[c++] = swap(state, z, z - 1);
    }
    if(z % 4 < 3) { // non-left column
        neighbors[c++] = swap(state, z, z + 1);
    }

    return c;
}

#endif