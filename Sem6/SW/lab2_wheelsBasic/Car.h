#include <Arduino.h>
#include <queue>
#include <functional>

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

    void update();
    bool busy();

    // controls
    void goForward(int cm);
    void goBack(int cm);
    void forward();
    void back();
    void left(int deg);
    void right(int deg);
    void stop();
    void keepGoing(int time_ms);
    void setSpeed(uint8_t speed);

  private:
    LiquidCrystal_I2C lcd;
    Wheels wheels;
    Dashboard dashboard;
    std::queue<std::function<bool()>> tasks;
    unsigned long last_update_time;
};

#endif