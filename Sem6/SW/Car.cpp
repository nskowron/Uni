#include <Arduino.h>

#include "Car.h"

#define DELAY_PER_CM_PER_SPEED 5000
#define MAX_SPEED 255

Car::Car(uint8_t LCD_addr,
    int rF, int rB, int rS,
    int lF, int lB, int lS
)   : lcd{LCD_addr, 16, 2}
    , dashboard{&wheels, &lcd}
{
    lcd.init();
    lcd.backlight();
    wheels.attach(rF, rB, rS, lF, lB, lS);
}

void Car::update() {
    dashboard.update();
    if(!tasks.empty()) {
        Command* task = tasks.top();
        if(task->call(&task->context)) {
            tasks.pop();
        }
    }
}

bool Car::busy() {
    return !tasks.empty();
}

void Car::goForward(int cm) {
    forward();
    tasks.push([this, cm, last_update_time] (Context* c) {
        // check speed
        uint8_t speed = wheels.speed_left > wheels.speed_right ? wheels.speed_left : wheels.speed_right;
        if(speed == 0) { // set max speed bc why not
            speed = MAX_SPEED;
        }
        wheels.setSpeed(speed);

        // calculate delay
        int goal_delay = DELAY_PER_CM_PER_SPEED / speed * c->cm;
        int elapsed_delay = millis() - last_update_time;
        
        // calculate distance to go
        int distance_to_go = std::max(c->cm - c->cm * elapsed_delay / goal_delay, 0);

        // update dashboard
        dashboard.update(distance_to_go);

        // check if done
        return elapsed_delay >= goal_delay;
    });
}

void Car::goBack(int cm) {
    back();
    tasks.push([&this, =cm, =last_update_time] () {
        // check speed
        uint8_t speed = this->speed_left > this->speed_right ? this->speed_left : this->speed_right;
        if(speed == 0) { // set max speed bc why not
            speed = MAX_SPEED;
        }
        wheels.setSpeed(speed);

        // calculate delay
        int goal_delay = DELAY_PER_CM_PER_SPEED / speed * cm;
        int elapsed_delay = millis() - last_update_time;
        
        // calculate distance to go
        int distance_to_go = std::max(cm - cm * elapsed_delay / goal_delay, 0);

        // update dashboard
        dashboard.update(distance_to_go);

        // check if done
        return elapsed_delay >= goal_delay;
    });
}

void Car::forward() {
    tasks.push([&this] () {
        wheels.forward();
        return true;
    });
}

void Car::back() {
    tasks.push([&this] () {
        wheels.back();
        return true;
    });
}

void Car::left(int deg) {
    // TODO
}

void Car::right(int deg) {
    // TODO
}

void Car::stop() {
    tasks.push([&this] () {
        wheels.stop();
        return true;
    });
}

void Car::keepGoing(int time_ms) {
    tasks.push([&this, =time_ms, =last_update_time] () {
        int elapsed_time = (millis() - last_update_time);
        return elapsed_time >= time_ms;
    });
}

void Car::setSpeed(uint8_t speed) {
    tasks.push([&this, =speed] () {
        wheels.setSpeed(speed);
        return true;
    });
}