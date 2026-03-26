#include <Arduino.h>

#include "Wheels.h"


#define SET_MOVEMENT(side,f,b) digitalWrite( side[0], f);\
                               digitalWrite( side[1], b)

#define DELAY_PER_CM_PER_SPEED 5000

Wheels::Wheels() 
{ }

void Wheels::attachRight(int pF, int pB, int pS)
{
    pinMode(pF, OUTPUT);
    pinMode(pB, OUTPUT);
    pinMode(pS, OUTPUT);
    this->pinsRight[0] = pF;
    this->pinsRight[1] = pB;
    this->pinsRight[2] = pS;
    this->setSpeedRight(0);
}


void Wheels::attachLeft(int pF, int pB, int pS)
{
    pinMode(pF, OUTPUT);
    pinMode(pB, OUTPUT);
    pinMode(pS, OUTPUT);
    this->pinsLeft[0] = pF;
    this->pinsLeft[1] = pB;
    this->pinsLeft[2] = pS;
    this->setSpeedLeft(0);
}

void Wheels::setSpeedRight(uint8_t s)
{
    this->speed_right = s;
    analogWrite(this->pinsRight[2], s);
}

void Wheels::setSpeedLeft(uint8_t s)
{
    this->speed_left = s;
    analogWrite(this->pinsLeft[2], s);
}

void Wheels::setSpeed(uint8_t s)
{
    setSpeedLeft(s);
    setSpeedRight(s);
}

void Wheels::attach(int pRF, int pRB, int pRS, int pLF, int pLB, int pLS, uint8_t LCD_addr)
{
    this->attachRight(pRF, pRB, pRS);
    this->attachLeft(pLF, pLB, pLS);
    this->lcd = new LiquidCrystal_I2C(LCD_addr, 16, 2);
    this->lcd->init();
    this->lcd->backlight();
}

void Wheels::forwardLeft() 
{
    SET_MOVEMENT(pinsLeft, HIGH, LOW);
}

void Wheels::forwardRight() 
{
    SET_MOVEMENT(pinsRight, HIGH, LOW);
}

void Wheels::backLeft()
{
    SET_MOVEMENT(pinsLeft, LOW, HIGH);
}

void Wheels::backRight()
{
    SET_MOVEMENT(pinsRight, LOW, HIGH);
}

void Wheels::forward()
{
    this->forwardLeft();
    this->forwardRight();
}

void Wheels::back()
{
    this->backLeft();
    this->backRight();
}

void Wheels::stopLeft()
{
    SET_MOVEMENT(pinsLeft, LOW, LOW);
}

void Wheels::stopRight()
{
    SET_MOVEMENT(pinsRight, LOW, LOW);
}

void Wheels::stop()
{
    this->stopLeft();
    this->stopRight();
}

void Wheels::goForward(int cm) {
  // equalize speed
  uint8_t speed = this->speed_left > this->speed_right ? this->speed_left : this->speed_right;
  if(speed == 0) { // set max speed bc why not
    speed = 255;
  }
  this->setSpeed(speed);

  // go forward
  this->lcd->clear();
  this->lcd->print(cm);
  this->forward();
  delay(DELAY_PER_CM_PER_SPEED / speed * cm);
  this->stop();
  this->lcd->clear();
  this->lcd->print(0);
}

void Wheels::goBack(int cm) {
  // equalize speed
  uint8_t speed = this->speed_left > this->speed_right ? this->speed_left : this->speed_right;
  if(speed == 0) { // set max speed bc why not
    speed = 255;
  }
  this->setSpeed(speed);

  // go back
  this->back();
  delay(DELAY_PER_CM_PER_SPEED / speed * cm);
  this->stop();
}


