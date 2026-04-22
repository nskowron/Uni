#include <Arduino.h>

#include "Queue.h"
#include "Wheels.h"
#include "Dashboard.h"
#include "Beeper.h"
#include "LiquidCrystal_I2C.h"

#ifndef Car_h
#define Car_h

class Car;

struct Context {
    Car* car;
    uint8_t speed;
    int cm;
    unsigned long timestamp;
};

struct Command {
    Context context;
    bool (*call)(Context*); 
};

class Car {
public:
    Car(uint8_t LCD_addr, int beep_pin,
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
    Beeper beeper;
    Dashboard dashboard;
    Queue<Command*> commands;
    unsigned long last_update_time = millis();
};

#endif