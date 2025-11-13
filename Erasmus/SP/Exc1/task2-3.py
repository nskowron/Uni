from task3 import gauss_seidel
from task2 import qr_decomp, A, b

print(f"QR: {qr_decomp(A, b)}")

# GS is only for square A so wtf
# print(f"GS: {gauss_seidel(A, b)}")