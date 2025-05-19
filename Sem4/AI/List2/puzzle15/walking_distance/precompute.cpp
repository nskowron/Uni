#include <cstdint>
#include <fstream>
#include <queue>
#include <unordered_set>

uint8_t get_neighbors(uint64_t state, uint64_t neighbors[8]);

int main(void) {
    std::ofstream db("db.bin", std::ios::binary);

    std::queue<uint64_t> to_search;
    std::unordered_set<uint64_t> searched;

    // start - zero_pos = 0 | distance = 0 | state
    to_search.emplace(((uint64_t)4 << 45) | ((uint64_t)4 << 30) | ((uint64_t)4 << 15) | (uint64_t)3);

    while(!to_search.empty()) {
        uint64_t current = to_search.front();
        to_search.pop();

        // we've been here - just state
        if(searched.find((current << 16) >> 16) != searched.end()) {
            continue;
        }

        // mark visited and write to file
        searched.insert((current << 16) >> 16);

        uint64_t state_dist = (current << 2) >> 2;
        db.write(reinterpret_cast<const char*>(&state_dist), sizeof(state_dist));

        // search neighbors
        uint64_t neighbors[8];
        uint8_t c = get_neighbors(current, neighbors);
        while(c-- > 0) {
            if(searched.find((neighbors[c] << 16) >> 16) == searched.end()) { // not visited
                to_search.push(neighbors[c]);
            }
        }
    }

    return 0;
}

inline uint64_t add(uint64_t state, uint8_t z, uint8_t x, uint8_t y) { // zero col, x col, y row for both
    uint64_t new_state = ((state << 16) >> 16) | ((uint64_t)x << 62) | ((((state >> 48) & 0xFFULL) + 1) << 48);
    uint8_t added = ((state >> (z * 12 + y * 3)) & 0b111ULL) + 1;
    uint8_t subtracted = ((state >> (x * 12 + y * 3)) & 0b111ULL) - 1;
    return (new_state & ~((0b111ULL << (z * 12 + y * 3)) | (0b111ULL << (x * 12 + y * 3)))) | ((uint64_t)added << (z * 12 + y * 3)) | ((uint64_t)subtracted << (x * 12 + y * 3));
}

inline uint8_t get_neighbors(uint64_t state, uint64_t neighbors[8]) {
    uint8_t z = state >> 62;

    uint8_t c = 0;
    if(z > 0) { // non-right "col"
        for(uint8_t i = 0; i < 4; ++i) { // all "rows"
            if(((state >> ((z - 1) * 12 + i * 3)) & 0b111ULL) > 0) { // there's tiles from that row to move
                neighbors[c++] = add(state, z, z - 1, i);
            }
        }
    }
    if(z < 3) { // non-left "col"
        for(uint8_t i = 0; i < 4; ++i) { // all "rows"
            if(((state >> ((z + 1) * 12 + i * 3)) & 0b111ULL) > 0) {
                neighbors[c++] = add(state, z, z + 1, i);
            }
        }
    }

    return c;
}