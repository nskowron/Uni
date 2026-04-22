#ifndef Speedometer_h
#define Speedometer_h

class Speedometer {
public:
    Speedometer();

    void update();
    uint32_t get_speed_left();
    uint32_t get_speed_right();

private:
    uint32_t speed_left;
    uint32_t speed_right;
    unsigned long last_update_time;
    unsigned long measure_time;
};

#endif