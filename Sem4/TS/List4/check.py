import sys

with open('Z.txt', 'r') as f1, open('Z_odtworzony.txt', 'r') as f2:
    z = f1.read().strip()
    z_odtw = f2.read().strip()

if len(z) != len(z_odtw):
    print(f'❌ Pliki mają różne długości: Z={len(z)} bitów, Z_odtworzony={len(z_odtw)} bitów')
    sys.exit(0)

for i in range(len(z)):
    if z[i] != z_odtw[i]:
        print(f'❌ Różnica na pozycji {i}: Z="{z[i]}", Z_odtworzony="{z_odtw[i]}"')
        sys.exit(0)

print('✅ Pliki są identyczne!')
