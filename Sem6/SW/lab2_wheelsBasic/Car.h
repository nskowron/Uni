#include <Arduino.h>

#include "Wheels.h"
#include "Dashboard.h"
#include "LiquidCrystal_I2C.h"

#ifndef Car_h
#define Car_h

class Car {
  public:
    Car(uint8_t LCD_addr,
        int pinRightForward, int pinRightBack, int pinRightSpeed,
        int pinLeftForward, int pinLeftBack, int pinLeftSpeed
    );

    Wheels& getWheels();

  private:
    LiquidCrystal_I2C lcd;
    Wheels wheels;
};

#endif