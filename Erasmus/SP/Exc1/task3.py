def gauss_seidel(A, b):
    m, n = A.shape
    x = [0] * n # initial guess, whatev

    epsilon = 1e-5
    while True:
        new_x = [0] * m
        for i in range(m):
            new_x[i] = (b[i] - sum(A[i][j] * x[j] for j in range(n) if j != i)) / A[i][i]
        if all(abs(new_x[i] - x[i]) < epsilon for i in range(m)):
            break
        x = new_x

