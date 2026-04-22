#include "Car.h"

Car c{0x27,13,4,3,5,2,7,6};
volatile char cmd;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  Serial.print("sanity check");

  c.setSpeed(200);
}

void loop() {
  c.update();
  if (!c.busy()) {
    c.goForward(10);
    c.goBack(10);
  }
}