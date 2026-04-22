#include <Arduino.h>

#include "Speedometer.h"

#define INTINPUT0 A0
#define INTINPUT1 A1

#define SPEED_PER_COUNTER_PER_MS 0 // TODO

static volatile unsigned int cnt0;
static volatile int unsigned cnt1;

ISR(PCINT1_vect){
    if( (PINC & (1 << PC0)) ) 
    cnt0++;

    if( (PINC & (1 << PC1)) )
    cnt1++;
}

Speedometer::Speedometer() 
    : last_update_time{millis()}
    , measure_time(100)
{
    pinMode(INTINPUT0, INPUT);
    pinMode(INTINPUT1, INPUT);

    PCICR  = 0x02;
    PCMSK1 = 0x03;
    
    update();
}

void Speedometer::update() {
    unsigned long current_time = millis();
    unsigned long elapsed_time = current_time - last_update_time;

    if (elapsed_time >= measure_time) {
        int cnt0_now = cnt0; cnt0 = 0;
        int cnt1_now = cnt1; cnt1 = 0;
        speed_left = cnt0 * SPEED_PER_COUNTER_PER_MS / elapsed_time;
        speed_right = cnt1 * SPEED_PER_COUNTER_PER_MS / elapsed_time;
        last_update_time = current_time;
    }
}

uint32_t Speedometer::get_speed_left() { return speed_left; }
uint32_t Speedometer::get_speed_right() { return speed_right; }