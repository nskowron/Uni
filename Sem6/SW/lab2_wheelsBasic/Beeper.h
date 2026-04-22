#include "Wheels.h"

#ifndef Beeper_h
#define Beeper_h

class Beeper {
public:
    Beeper(const Wheels* wheels, int beep_pin);

    void update();

private:
    Wheels* wheels;
    int beep_pin;
    unsigned long last_update_time;
};

#endif