def bit_unstuffing(data):
    count = 0
    unstuffed = ''
    i = 0
    while i < len(data):
        bit = data[i]
        unstuffed += bit
        if bit == '1':
            count += 1
            if count == 5:
                i += 1
                count = 0
        else:
            count = 0
        i += 1
    return unstuffed

def compute_crc(data, generator='100000111'):
    data += '0' * (len(generator) - 1)
    data = list(data)
    gen = list(generator)
    for i in range(len(data) - len(generator) + 1):
        if data[i] == '1':
            for j in range(len(generator)):
                data[i + j] = str(int(data[i + j]) ^ int(gen[j]))
    return ''.join(data[-(len(generator) - 1):])

def extract_frames(bitstream):
    FLAG = '01111110'
    frames = []
    start = bitstream.find(FLAG, 0)
    if start == -1:
        return frames
    while start < len(bitstream):
        end = bitstream.find(FLAG, start + len(FLAG))
        while end == start + len(FLAG):  # empty frame
            start = end
            end = bitstream.find(FLAG, start + len(FLAG))
        if end == -1:
            break
        frame = bitstream[start + len(FLAG):end]
        frames.append(frame)
        start = end
    return frames

def decode_frame(frame):
    unstuffed = bit_unstuffing(frame)
    crc_len = 8  # for CRC-8
    data = unstuffed[:-crc_len]
    crc = unstuffed[-crc_len:]
    check = compute_crc(data)
    if check == crc:
        return data
    else:
        raise ValueError('Wrong CRC')

def main():
    with open('W', 'r') as f:
        framed_data = f.read().strip()

    frames = extract_frames(framed_data)
    recovered = ''

    for i, frame in enumerate(frames):
        try:
            segment = decode_frame(frame)
            recovered += segment
        except ValueError:
            print(f'Błąd CRC w ramce {i + 1}')

    with open('Z_recovered', 'w') as f:
        f.write(recovered)

if __name__ == '__main__':
    main()