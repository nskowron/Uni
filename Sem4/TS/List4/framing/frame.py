def bit_stuffing(data):
    count = 0
    stuffed = ''
    for bit in data:
        stuffed += bit
        if bit == '1':
            count += 1
            if count == 5:
                stuffed += '0'
                count = 0
        else:
            count = 0
    return stuffed

def compute_crc(data, generator='100000111'):  # CRC-8
    data += '0' * (len(generator) - 1)
    data = list(data)
    gen = list(generator)
    for i in range(len(data) - len(generator) + 1):
        if data[i] == '1':
            for j in range(len(generator)):
                data[i + j] = str(int(data[i + j]) ^ int(gen[j]))
    return ''.join(data[-(len(generator) - 1):])

def encode_frame(data):
    crc = compute_crc(data)
    full_data = data + crc
    stuffed = bit_stuffing(full_data)
    return '01111110' + stuffed + '01111110'

def split_into_chunks(bitstream, chunk_size):
    return [bitstream[i:i+chunk_size] for i in range(0, len(bitstream), chunk_size)]

def main():
    with open('Z', 'r') as f:
        bitstream = f.read().strip()

    chunk_size = 512
    chunks = split_into_chunks(bitstream, chunk_size)

    with open('W', 'w') as f:
        for chunk in chunks:
            frame = encode_frame(chunk)
            f.write(frame)

if __name__ == '__main__':
    main()
