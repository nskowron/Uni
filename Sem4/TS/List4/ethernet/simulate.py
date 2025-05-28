from copy import copy
import random
import time

ETHERNET_CABLE_LENGHT = 20

class CableCell:
    def __init__(self, _pos_):
        self.pos = _pos_
        self.left = None
        self.right = None

    def new_signal(self, signal):
        self.left = signal if self.left == None else '#'
        self.right = signal if self.right == None else '#'

    def propagate(self, cable, new_cable):
        new_self = new_cable[self.pos]

        if self.pos != 0: # have left neighbor
            left = cable[self.pos-1]
            new_left = new_cable[self.pos-1]
            if self.left != None and left.right != None:
                new_left.left = '#'
                new_self.right = '#'
            elif left.right != None:
                new_self.right = left.right
            elif self.left != None:
                new_left.left = self.left
        
        if self.pos != ETHERNET_CABLE_LENGHT - 1: # have right neighbor
            right = cable[self.pos+1]
            new_right = new_cable[self.pos+1]
            if self.right != None and right.left != None:
                new_right.right = '#'
                new_self.left = '#'
            elif right.left != None:
                new_self.left = right.left
            elif self.right != None:
                new_right.right = self.right
        
        if new_self.left != None and new_self.right != None:
            new_self.left = '#'
            new_self.right = '#'
    
    def __str__(self):
        if self.left == None and self.right == None:
            return '_'
        return self.right if self.left == None else self.left
    
class Transmission:
    def __init__(self, _src_, _pos_, _len_):
        self.src = _src_                        # A, B or C
        self.pos = _pos_                        # pos in the cable
        self.len = _len_                        # length of the packet
        self.left = _len_                       # how much of the packet is left to transmit
        self.wait = ETHERNET_CABLE_LENGHT       # wait to make sure there were no collisions
        self.sleep = 0                          # sleep after a collision

    def transmit(self, cable):
        if self.wait == 0:
            return True
        
        if self.sleep > 0:
            self.sleep -= 1
            return False

        if cable[self.pos].left == '#' or cable[self.pos].right == '#':
            self.sleep = random.choice([1, 2]) * ETHERNET_CABLE_LENGHT
            self.wait = ETHERNET_CABLE_LENGHT
            self.left = self.len
            return False

        if self.left == 0:
            self.wait -= 1
        elif cable[self.pos].left == None and cable[self.pos].right == None: #clear
            cable[self.pos].new_signal(self.src)
            self.left -= 1
        
        return False

class Device:
    def __init__(self, _name_, _pos_, _rounds_):
        self.name = _name_
        self.pos = _pos_
        self.round = 0
        self.transmission = None
        self.transmissions = [[r, Transmission(_name_, _pos_, random.randint(5, 10))] for r in _rounds_]
    
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

def main():
    cable = [CableCell(i) for i in range(ETHERNET_CABLE_LENGHT)]

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

        new_cable = [CableCell(i) for i in range(ETHERNET_CABLE_LENGHT)]
        for cell in cable:
            cell.propagate(cable, new_cable)
        cable = new_cable
        devices = [d for d in devices if d.refresh(cable)]

        # print("\033[2J\033[H", end="") # clear
        print(f"ROUND: {current_round}")
        print("".join([str(cell) for cell in cable]))

        device_line = [' '] * ETHERNET_CABLE_LENGHT
        for dev in devices:
            device_line[dev.pos] = str(dev)
        print("".join(device_line))

        time.sleep(0.5)

if __name__ == '__main__':
    main()