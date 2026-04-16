#ifndef Dashboard_h
#define Dashboard_h

#include "LiquidCrystal_I2C.h"

#include "Wheels.h"

class Dashboard {
  public:
    Dashboard(const Wheels* wheels, LiquidCrystal_I2C* lcd);

    void update();
    void update(int distance);

  private:
    Wheels* wheels;
    LiquidCrystal_I2C* lcd;
    unsigned long last_update_time = millis();
    unsigned long animation_frame_time;
    bool animation_frame;
};

#endif