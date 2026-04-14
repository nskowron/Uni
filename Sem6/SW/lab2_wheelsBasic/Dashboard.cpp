#include <Arduino.h>
#include <time.h>

#include "LiquidCrystal_I2C.h"
#include "Dashboard.h"
#include "Wheels.h"

Dashboard::Dashboard(Wheels* wheels) 
    : wheels{wheels}
    , last_update_time{millis()} 
{
    update();
}

void Dashboard::update() {
    // animation, speed, direction
}

void Dashboard::update(int distance) {
    // distance to go
}