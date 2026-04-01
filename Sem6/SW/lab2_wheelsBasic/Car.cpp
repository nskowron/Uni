#include <Arduino.h>

#include "Car.h"

Car::Car(uint8_t LCD_addr,
    int rF, int rB, int rS,
    int lF, int lB, int lS
) : lcd(LCD_addr, 16, 2) {
    lcd.init();
    lcd.backlight();
    wheels.attach(rF, rB, rS, lF, lB, lS);

    // start dashboard thread
}

Wheels& Car::getWheels() {
    return wheels;
}