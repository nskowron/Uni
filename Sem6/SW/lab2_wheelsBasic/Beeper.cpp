#include <Arduino.h>

#include "Beeper.h"

#define BEEP_TIME_PER_SPEED 50000

Beeper::Beeper(const Wheels* wheels, int beep_pin = 13)
    : wheels{wheels}
    , beep_pin(beep_pin)
{}

void Beeper::update() {
    short total_speed = (wheels->speed_left * wheels->direction_left + wheels->speed_right * wheels->direction_right) / 2;

    if (total_speed < 0) {
        unsigned long current_time = millis();
        unsigned long goal_delay = BEEP_TIME_PER_SPEED / total_speed;

        if (current_time - last_update_time >= goal_delay) {
            digitalWrite(beep_pin, digitalRead(beep_pin)^1);
            last_update_time = current_time;
        }
    } else {
        digitalWrite(beep_pin, LOW);
    }
}