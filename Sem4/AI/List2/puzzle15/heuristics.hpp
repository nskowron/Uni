#ifndef HEURISTICS_HPP
#define HEURISTICS_HPP

#include <cstdint>

inline uint8_t manhattan(uint64_t state) {
    uint8_t distance = 0;
    for(int i = 0; i < 16; ++i) {
        int tile = (state >> (i * 4)) & 0xF;
        if(tile == 0) {
            continue;
        }
        distance += abs((15 - i) / 4 - (tile - 1) / 4); // row
        distance += abs((15 - i) % 4 - (tile - 1) % 4); // col
    }
    return distance;
}

#endif