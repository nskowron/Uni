// newagent.c
// Nel Skowronek 279679

#include "agents.h"

struct agent newagent(int x, int y)
{
    struct agent a = {x, y};
    return a;
}