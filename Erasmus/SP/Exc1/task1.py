import numpy as np

D = np.array([
    [5, 110],
    [7, 114],
    [9, 118],
    [4, 108],
])

m = D.shape[0]
x = D[:, 0]

A = np.column_stack((
    np.ones(m),
    x,
    x**2,
))

print(A)

# similarly for second subtask
# can also make a function that takes a function of observations