import sys

with open('Z', 'r') as f1, open('Z_recovered', 'r') as f2:
    z = f1.read().strip()
    z_rec = f2.read().strip()

if len(z) != len(z_rec):
    print(f'Different lengths: Z={len(z)} bits, Z_recovered={len(z_rec)} bits')
    sys.exit(0)

for i in range(len(z)):
    if z[i] != z_rec[i]:
        print(f'Difference on position {i}: Z="{z[i]}", Z_recovered="{z_rec[i]}"')
        sys.exit(0)

print('Files are identical<3')
