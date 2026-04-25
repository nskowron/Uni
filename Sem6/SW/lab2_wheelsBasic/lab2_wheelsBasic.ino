#include "Car.h"

Car* c = nullptr; // {0x27,13,4,3,5,2,7,6};
volatile char cmd;
uint8_t speed;

void setup() {
  Serial.begin(9600);
  c = new Car{0x27,13,4,3,5,2,7,6};

  // put your setup code here, to run once:
  
  Serial.println("sanity check");

  speed = 0;
  c->setSpeed(speed);
}

void loop() {
  delay(50);
  // Serial.println("updating");
  c->update();
  if (!c->busy()) {
    Serial.println("enqueuing");
    c->goForward(10);
    c->goBack(10);
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