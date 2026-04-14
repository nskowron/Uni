#ifndef Dashboard_h
#define Dashboard_h

#include "Wheels.h"

class Dashboard {
  public:
    Dashboard(Wheels* wheels);

    void update();
    void update(int distance);

  private:
    Wheels* wheels;
    unsigned long last_update_time;
};

#endif