// distance.c
// Nel Skowronek 279679

#include "agents.h"
#include <math.h>

double distance(struct agent a1, struct agent a2)
{
    return sqrt((a1.y - a2.y)*(a1.y - a2.y) + (a1.x - a2.x)*(a1.x - a2.x));
}