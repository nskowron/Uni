#include <Arduino.h>

#include "Car.h"

#define DELAY_PER_CM_PER_SPEED 5000
#define MAX_SPEED 255

Car::Car(uint8_t LCD_addr, int beep,
    int rF, int rB, int rS,
    int lF, int lB, int lS
)   : lcd{LCD_addr, 16, 2}
    , dashboard{&wheels, &lcd}
    , beeper{&wheels, beep}
{
    wheels.attach(rF, rB, rS, lF, lB, lS);
}

void Car::update() {
    dashboard.update();
    beeper.update();
    speedometer.update();
    Serial.println("car update");
    if (!commands.empty()) {
        Serial.println("running command");
        Command* command = commands.top();
        if(command->call(&command->context)) {
            Serial.println("command finished");
            commands.pop();
        }
    }
}

bool Car::busy() {
    return !commands.empty();
}

void Car::goForward(int cm) {
    commands.push(new Command{
        Context{this, 0, cm, 0},
        [](Context* c){
            // check speed
            auto& [car, _, cm, timestamp] = *c;
            auto& wheels = car->wheels;
            uint8_t speed = wheels.speed_left > wheels.speed_right ? wheels.speed_left : wheels.speed_right;

            if (timestamp == 0) { // uninitialized
                timestamp = millis();

                if (speed == 0) { // set max speed bc why not
                    speed = MAX_SPEED;
                }
                wheels.setSpeed(speed);
                wheels.forward();
            }

            // calculate delay
            unsigned long goal_delay = cm * DELAY_PER_CM_PER_SPEED / speed;
            unsigned long elapsed_delay = millis() - timestamp;

            Serial.println(goal_delay);
            Serial.println(elapsed_delay);
            
            // calculate distance to go
            int distance_to_go = max(cm - cm * elapsed_delay / goal_delay, 0);

            // update dashboard
            car->dashboard.update(distance_to_go);

            // check if done
            return elapsed_delay >= goal_delay;
    }});
}

void Car::goBack(int cm) {
    commands.push(new Command{
        Context{this, 0, cm, 0},
        [](Context* c){
            // check speed
            auto& [car, _, cm, timestamp] = *c;
            auto& wheels = car->wheels;
            uint8_t speed = wheels.speed_left > wheels.speed_right ? wheels.speed_left : wheels.speed_right;

            if (timestamp == 0) { // uninitialized
                timestamp = millis();

                if (speed == 0) { // set max speed bc why not
                    speed = MAX_SPEED;
                }
                wheels.setSpeed(speed);
                wheels.back();
            }

            // calculate delay
            unsigned long goal_delay = cm * DELAY_PER_CM_PER_SPEED / speed;
            unsigned long elapsed_delay = millis() - timestamp;

            Serial.println(goal_delay);
            Serial.println(elapsed_delay);
            
            // calculate distance to go
            int distance_to_go = max(cm - cm * elapsed_delay / goal_delay, 0);

            // update dashboard
            car->dashboard.update(distance_to_go);

            // check if done
            return elapsed_delay >= goal_delay;
    }});
}

void Car::forward() {
    commands.push(new Command{
        Context{this, 0, 0, 0},
        [](Context* c){
            auto& [car, _1, _2, _3] = *c;
            auto& wheels = car->wheels;

            wheels.forward();

            return true;
    }});
}

void Car::back() {
    commands.push(new Command{
        Context{this, 0, 0, 0},
        [](Context* c){
            auto& [car, _1, _2, _3] = *c;
            auto& wheels = car->wheels;

            wheels.back();
            return true;
    }});
}

void Car::left(int deg) {
    // TODO
}

void Car::right(int deg) {
    // TODO
}

void Car::stop() {
    commands.push(new Command{
        Context{this, 0, 0, 0},
        [](Context* c){
            auto& [car, _1, _2, _3] = *c;
            auto& wheels = car->wheels;

            wheels.stop();
            return true;
    }});
}

void Car::keepGoing(int time_ms) {
    commands.push(new Command{
        Context{this, 0, time_ms, 0},
        [](Context* c){
            auto& [car, _, time_ms, timestamp] = *c;

            if (timestamp == 0) { // uninitialized
                timestamp = millis();
            }

            // calculate delay
            int elapsed_delay = millis() - timestamp;

            // check if done
            return elapsed_delay >= time_ms;
    }});
}

void Car::setSpeed(uint8_t speed) {
    commands.push(new Command{
        Context{this, speed, 0, 0},
        [](Context* c){
            auto& [car, speed, _1, _2] = *c;
            auto& wheels = car->wheels;

            wheels.setSpeed(speed);
            return true;
    }});
}