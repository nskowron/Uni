import socket
import struct
import numpy as np

from simulation import simulate

def receive_matrix(sock, n):
    size = n * n * 4  # 4 bytes per int
    data = b''
    while len(data) < size:
        chunk = sock.recv(size - len(data))
        if not chunk:
            raise ConnectionError("Socket closed while reading matrix.")
        data += chunk
    return np.frombuffer(data, dtype='>i4').reshape((n, n))


if __name__ == "__main__":
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.connect(('localhost', 2137))
        while True:
            dim_raw = s.recv(4) # matrix dimensions raw bytes
            if not dim_raw: # connection closed
                break
            (n,) = struct.unpack('!I', dim_raw) # unpack to int

            capacities = receive_matrix(s, n)
            intensities = receive_matrix(s, n)

            result = simulate(capacities, intensities)
            s.sendall(struct.pack('>d', result)) # send result as double

