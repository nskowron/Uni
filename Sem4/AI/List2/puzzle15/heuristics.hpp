#ifndef HEURISTICS_HPP
#define HEURISTICS_HPP

#include <cstdint>
#include <unordered_map>
#include <fstream>

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

inline uint8_t manhattan_linear(uint64_t state) {
    uint8_t distance = 0;
    for(int8_t i = 0; i < 16; ++i) {
        uint8_t tile = (state >> (i * 4)) & 0xFULL;
        if(tile == 0) {
            continue;
        }

        // Manhattan Distance
        distance += abs((15 - i) / 4 - (tile - 1) / 4); // row
        distance += abs((15 - i) % 4 - (tile - 1) % 4); // col

        // Linear Conflict
        for(int8_t j = i - 1; j >= 0 && j % 4 < 3; --j) { // row
            uint8_t other = (state >> (j * 4)) & 0xFULL;
            if((other != 0) && ((other - 1) / 4 == (tile - 1) / 4) && (other < tile)) { // there's a conflict
                distance += 2;
                break;
            }
        }
        for(int8_t j = i - 4; j >= 0; j -= 4) { // col
            uint8_t other = (state >> (j * 4)) & 0xFULL;
            if((other != 0) && (other % 4 == tile % 4) && (other < tile)) {
                distance += 2;
                break;
            }
        }
    }
    return distance;
}

std::unordered_map<uint64_t, uint8_t> WD;

void WD_load() {
    std::ifstream db("walking_distance/db.bin", std::ios::binary);
    uint64_t state_dist;
    while(db.read(reinterpret_cast<char*>(&state_dist), sizeof(state_dist))) {
        WD[(state_dist << 16) >> 16] = (state_dist >> 48);
        // std::cout << "db: " << state_dist << std::endl;
    }
}

inline uint8_t walking_distance(uint64_t state) {
    
    // Columns
    uint64_t col_state = 0;
    for(int8_t i = 0; i < 4; ++i) { // col
        for(uint8_t j = 0; j < 4; ++j) { // row
            uint8_t tile = (state >> (j * 16 + i * 4)) & 0xFULL;
            if(tile == 0) {
                continue;
            }

            uint8_t state_index = i * 12 + (3 - ((tile - 1) % 4)) * 3; // current_col x goal_col
            col_state = (col_state & ~((0b111ULL << state_index))) | ((((col_state >> state_index) & 0b111ULL) + 1) << state_index);
        }
    }

    // Rows
    uint64_t row_state = 0;
    for(int8_t i = 0; i < 4; ++i) { // col
        for(uint8_t j = 0; j < 4; ++j) { // row
            uint8_t tile = (state >> (j * 16 + i * 4)) & 0xFULL;
            if(tile == 0) {
                continue;
            }

            uint8_t state_index = j * 12 + (3 - ((tile - 1) / 4)) * 3; // current_col x goal_col
            row_state = (row_state & ~((0b111ULL << state_index))) | ((((row_state >> state_index) & 0b111ULL) + 1) << state_index);
        }
    }

    if(WD.find(col_state) == WD.end() || WD.find(row_state) == WD.end()) {
        std::cout << "whoopsie daisy: " << std::endl;
        std::cout << col_state << std::endl;
        std::cout << row_state << std::endl;
        return 0;
    }
    return WD.find(col_state)->second + WD.find(row_state)->second;
}

#endif