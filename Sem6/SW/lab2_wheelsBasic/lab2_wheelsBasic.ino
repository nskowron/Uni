#include "Car.h"

Car c{0x27,13,4,3,5,2,7,6};
volatile char cmd;
uint8_t speed;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  Serial.println("sanity check");

  speed = 0;
  c.setSpeed(speed);
}

void loop() {
  c.update();
  if (!c.busy()) {
    c.goForward(10);
    c.goBack(10);
  }
}

// void loop() {
//     c.update();
//     if (!c.busy()) {
//         uint32_t speed_left = c.speedometer.get_speed_left();
//         uint32_t speed_right = c.speedometer.get_speed_right();
//         Serial.print(speed);
//         Serial.print(" ");
//         Serial.print(speed_left);
//         Serial.print(" ");
//         Serial.println(speed_right);

//         speed += 10;
//         c.setSpeed(speed);
//         c.keepGoing(200);
//     }
// }