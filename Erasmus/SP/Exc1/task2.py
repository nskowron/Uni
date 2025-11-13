import scipy.linalg as la
import numpy as np

def qr_decomp(A, b):
    Q, R = la.qr(A, mode='economic')
    return la.solve_triangular(R, Q.T @ b)

A = np.array([
    [0, 1],
    [2, 1],
    [2, 1],
])

b = np.array([
    1, 1, 2,
])

beta_hat = qr_decomp(A, b)
print(f"beta_hat: {beta_hat}")
print(f"b estimate: {A @ beta_hat}")

# calculate by hand

def project(v, u):
    return (np.dot(v, u) / np.dot(u, u)) * u

def gram_schmidt(A):
    m, n = A.shape
    Q = np.zeros((m, n))
    for j in range(n):
        Q[:, j] = A[:, j] - sum(project(A[:, j], Q[:, i]) for i in range(j))
        Q[:, j] = Q[:, j] / np.linalg.norm(Q[:, j])
    return Q

def my_qr(A):
    Q = gram_schmidt(A)
    R = Q.T @ A
    return Q, R

def qr_decomp_manual(A, b):
    Q, R = my_qr(A)
    return la.solve_triangular(R, Q.T @ b)

print(f"Manual QR beta_hat: {qr_decomp_manual(A, b)}")