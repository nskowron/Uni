#include <Arduino.h>
#include <time.h>

#include "LiquidCrystal_I2C.h"
#include "Dashboard.h"
#include "Wheels.h"

#define ARR_UP_2 0
#define ARR_DOWN_2 1
#define ARR_UP_1 3
#define ARR_DOWN_1 4
#define ARR_STOP 2

uint8_t arr_up_2[8] = {
    0b00100,
    0b01110,
    0b11011,
    0b10001,
    0b00100,
    0b01110,
    0b11011,
    0b10001
}

uint8_t arr_down_2[8] = {
    0b10001,
    0b11011,
    0b01110,
    0b00100,
    0b10001,
    0b11011,
    0b01110,
    0b00100
};

uint8_t arr_up_1[8] = {
    0b00000,
    0b00000,
    0b00100,
    0b01110,
    0b11011,
    0b10001,
    0b00000,
    0b00000
};

uint8_t arr_down_1[8] = {
    0b00000,
    0b00000,
    0b10001,
    0b11011,
    0b01110,
    0b00100,
    0b00000,
    0b00000
};

uint8_t arr_stop[8] = {
    0b00000,
    0b00000,
    0b00000,
    0b11111,
    0b11111,
    0b00000,
    0b00000,
    0b00000
};

char* animation_frame_1[2] = {
    " /|\\ ",
    "/   \\"
};

char* animation_frame_2[2] = {
    " / \\ ",
    "/ | \\"
};

Dashboard::Dashboard(Wheels* wheels, LiquidCrystal_I2C* lcd) 
    : wheels{wheels}
    , lcd{lcd}
    , last_update_time{millis()}
    , animation_frame_time{100}
    , animation_frame{0}
{
    lcd->createChar(ARR_UP_2, arr_up_2);
    lcd->createChar(ARR_DOWN_2, arr_down_2);
    lcd->createChar(ARR_UP_1, arr_up_1);
    lcd->createChar(ARR_DOWN_1, arr_down_1);
    lcd->createChar(ARR_STOP, arr_stop);
    update(0);
}

void Dashboard::update() {
    unsigned long current_time = millis();

    // animation
    if (current_time - animation_frame_time >= 100) {
        animation_frame = !animation_frame;
        char** frame = animation_frame ? animation_frame_1 : animation_frame_2;
        lcd->setCursor(6, 0);
        lcd->print(frame[0]);
        lcd->setCursor(6, 1);
        lcd->print(frame[1]);
    }

    // speed
    short total_speed = (wheels->speed_left * wheels->direction_left + wheels->speed_right * wheels->direction_right) / 2;
    if (total_speed == 0) {
        lcd->setCursor(15, 0);
    } else if (total_speed > 0) {
        lcd->setCursor(total_speed < 10 ? 14 : total_speed < 100 ? 13 : 12, 0);
        lcd->print("+");
    } else {
        lcd->setCursor(total_speed > -10 ? 14 : total_speed > -100 ? 13 : 12, 0);
        lcd->print("-");
        total_speed = -total_speed; // quick abs
    }
    lcd->print(total_speed);

    // engines
    lcd->setCursor(0, 1);
    if (wheels->direction_left == 0 || wheels->speed_left == 0) {
        lcd->write(ARR_STOP);
    } else if (wheels->direction_left > 0) {
        if (wheels->speed_left < 150) {
            lcd->write(ARR_UP_1);
        } else {
            lcd->write(ARR_UP_2);
        }
    } else {
        if (wheels->speed_left < 150) {
            lcd->write(ARR_DOWN_1);
        } else {
            lcd->write(ARR_DOWN_2);
        }
    }

    lcd->setCursor(15, 1);
    if (wheels->direction_right == 0 || wheels->speed_right == 0) {
        lcd->write(ARR_STOP);
    } else if (wheels->direction_right > 0) {
        if (wheels->speed_right < 150) {
            lcd->write(ARR_UP_1);
        } else {
            lcd->write(ARR_UP_2);
        }
    } else {
        if (wheels->speed_right < 150) {
            lcd->write(ARR_DOWN_1);
        } else {
            lcd->write(ARR_DOWN_2);
        }
    }

    last_update_time = current_time;
}

void Dashboard::update(int distance) {
    lcd->setCursor(0, 0);
    lcd->print(distance);

    update();
}