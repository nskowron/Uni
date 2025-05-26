import random

with open('Z', 'w') as f:
    f.write(''.join(random.choice('01') for _ in range(2048)))