import numpy

# exc1

# x + y = 6300
# 0.045x + 0.0375y = 267.75
# solve however we like

A1 = [
    [1, 1],
    [0.045, 0.0375]
]
b1 = [6300, 267.75]
x1, y1 = numpy.linalg.solve(A1, b1)

print(f"x = {x1}, y = {y1}")


# exc2

# if we multiply the 5x5 matrix by the vector of amounts of products made
# (transposing correctly and multiplying in the right order)
# we will get the vector of the amounts of each material used

p_1={"metal":0, "concrete":1.3, "plastic":0.2, "water":.8, "electricity":.4}
p_2={"metal":0, "concrete":0, "plastic":1.5, "water":.4, "electricity":.3}
p_3={"metal":.25, "concrete":0, "plastic":0, "water":.2, "electricity":.7}
p_4={"metal":0, "concrete":0, "plastic":.3, "water":.7, "electricity":.5}
p_5={"metal":1.5, "concrete":0, "plastic":.5, "water":.4, "electricity":.8}

A2 = [
    list(p_1.values()),
    list(p_2.values()),
    list(p_3.values()),
    list(p_4.values()),
    list(p_5.values())
]
x2 = [10, 9, 12, 13, 11]
b2 = x2 @ A2

print(f"metal used: {b2[0]}") # metal is first


# exc3

# we have the same equation as above but this time we have A and b
# so we need to solve for x: xA = b

# if rank A = 5: there will be exactly one unique solution
# else: the row space of A is smaller than R5, so
# we might have no solution (if b is outside A row space)
# or we might have infinitely many (an entire (5-rankA)-dimentional space of solutions)