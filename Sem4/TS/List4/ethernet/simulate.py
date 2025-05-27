from copy import copy
import random
import time

ETHERNET_CABLE_LENGHT = 20

class Signal:
    def __init__(self, _src_, _dir_):
        self.src = _src_
        self.dir = _dir_

    def prop_left(self):
        signal = copy(self)
        signal.dir = -1
        return signal
    
    def prop_right(self):
        signal = copy(self)
        signal.dir = 1
        return signal
    
    def __str__(self):
        return self.src
    
class Transmission:
    def __init__(self, _src_, _pos_, _len_):
        self.src = _src_                        # A, B or C
        self.pos = _pos_                        # pos in the cable
        self.len = _len_                        # length of the packet
        self.left = _len_                       # how much of the packet is left to transmit
        self.wait = ETHERNET_CABLE_LENGHT       # wait to make sure there were no collisions
        self.sleep = 0                          # sleep after a collision

    def transmit(self, signals):
        if self.wait == 0:
            return True
        
        if self.sleep > 0:
            self.sleep -= 1
            return False

        if signals[self.pos] != None and signals[self.pos].src == '#':
            self.sleep = random.choice([1, 2]) * ETHERNET_CABLE_LENGHT
            self.wait = ETHERNET_CABLE_LENGHT
            self.left = self.len
            return False

        if self.left == 0:
            self.wait -= 1
            if signals[self.pos] != None and signals[self.pos].src == self.src:
                signals[self.pos] = None
        else:
            signals[self.pos] = Signal(self.src, 0)
            self.left -= 1
        
        return False

class Device:
    def __init__(self, _name_, _pos_, _rounds_):
        self.name = _name_
        self.round = 0
        self.transmission = None
        self.transmissions = [[r, Transmission(_name_, _pos_, random.randint(1, 3) * (ETHERNET_CABLE_LENGHT // 2))] for r in _rounds_]
    
    def refresh(self, cable):
        self.round += 1

        if self.transmission != None:
            if self.transmission.transmit(cable):
                self.transmission = None
            else:
                return True
        
        if self.transmissions:
            r, t = self.transmissions[0]
            if self.round >= r:
                self.transmission = t
                self.transmissions = self.transmissions[1:]
                self.transmission.transmit(cable)
            return True
        else:
            return False

    def __str__(self):
        return self.name
    

def propagate(signals):
    new_signals = copy(signals)
    for i in range(len(signals)-1):
        left = signals[i]
        right = signals[i+1]

        if left == None and right == None:                  # [_,_] -> [_,_]
            continue
        if right == None:
            if left.dir == -1:                              # [<,_] -> [_,_]
                new_signals[i] = None
            else:                                           # [>,_] -> [>,>]
                new_signals[i+1] = left.prop_right()
        elif left == None:
            if right.dir == 1:                              # [_,>] -> [_,_]
                new_signals[i+1] = None
            elif new_signals[i] != None:                    # [N,<] -> [#,<]
                new_signals[i] = Signal('#', 0)
            else:
                new_signals[i] = right.prop_left()          # [_,<] -> [<,<]
        else:
            if left.dir == -1 and right.dir == 1:           # [<,>] -> [_,_]
                new_signals[i] = None
                new_signals[i+1] = None
            else:
                if left.src == right.src:                   # [A,A] -> [A,A]
                    continue
                if left.dir == -1:                          # [<,<] -> [<,<]
                    new_signals[i] = right.prop_left()
                elif right.dir == 1:                        # [>,>] -> [>,>]
                    new_signals[i+1] = left.prop_right()
                else:                                       # [>,<] -> [#,#]
                    new_signals[i] = Signal('#', -1)
                    new_signals[i+1] = Signal('#', 1)
    
    return new_signals

def main():
    cable = [None] * ETHERNET_CABLE_LENGHT

    devices = [
        Device('A', 3,
               [1, 40, 41]),
        Device('B', 9,
               [50]),
        Device('C', 15,
               [55, 60, 80])
    ]

    current_round = 0

    while devices:
        current_round += 1

        cable = propagate(cable)
        devices = [d for d in devices if d.refresh(cable)]

        print("\033[2J\033[H", end="")
        print(f"ROUND: {current_round}")
        print("".join(str(s) if s is not None else "_" for s in cable))
        print("".join(str(d) if d is not None else " " for d in devices))

        time.sleep(0.5)

if __name__ == '__main__':
    main()