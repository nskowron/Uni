#ifndef STATE_DATA_HPP
#define STATE_DATA_HPP

#include <cstdint>

struct state_data {
    uint64_t state; // 48 bits for state
    uint16_t data; // 8bits for distance | 8bits for zero pos

    inline uint8_t zero_pos() const {
        return data & 0xFF;
    }
    inline uint8_t dist() const {
        return data >> 8;
    }
    inline uint64_t compress() const { // write to db
        return state | ((uint64_t)(data >> 8) << 48);
    }
    inline static state_data decompress(uint64_t compressed) { // read from db
        return state_data((compressed << 16) >> 16, (uint8_t)((compressed >> 48) & 0xFFULL), 0);
    }

    state_data(uint64_t s, uint8_t d, uint8_t z) :
        state(s),
        data(((uint16_t)d << 8) | (uint16_t)z)
    {}
};

#endif