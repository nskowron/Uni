#include "Car.h"

Car* c;
volatile char cmd;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  Serial.print("sanity check0");

  c = new Car{0x27,4,3,5,2,7,6};
}

void loop() {
  // while(Serial.available())
  // {
  //   cmd = Serial.read();
  //   switch(cmd)
  //   {
  //     case 'w': w.forward(); break;
  //     case 'x': w.back(); break;
  //     case 'a': w.forwardLeft(); break;
  //     case 'd': w.forwardRight(); break;
  //     case 'z': w.backLeft(); break;
  //     case 'c': w.backRight(); break;
  //     case 's': w.stop(); break;
  //     case '1': w.setSpeedLeft(75); break;
  //     case '2': w.setSpeedLeft(200); break;
  //     case '9': w.setSpeedRight(75); break;
  //     case '0': w.setSpeedRight(200); break;
  //     case '5': w.setSpeed(200); break;
  //   }
  // }
  Serial.write("sanity check");

  c->getWheels().setSpeedLeft(100);
  c->getWheels().setSpeedRight(200);

  Serial.println(c->getWheels().speed_left);
       
  c->getWheels().goForward(10);
  delay(1000);

  c->getWheels().goBack(10);
  delay(1000);
}
