import random

with open('Z.txt', 'w') as f:
    f.write(''.join(random.choice('01') for _ in range(2048)))