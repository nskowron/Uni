from scapy.all import sniff, IP_PROTOS
import time
import signal
import sys

def sigint_handler(signal, frame):
    sys.exit(0)

signal.signal(signal.SIGINT, sigint_handler)

max_lines = 20
log_lines = []

no = 1
start_time = time.time()

while True:
    packet = sniff(count=1)[0]
    line = " | ".join([
        f"{no}",
        f"{(time.time() - start_time):.5f}",
        f"{packet['IP'].src if packet.haslayer('IP') else packet.src}",
        f"{packet['IP'].dst if packet.haslayer('IP') else packet.dst}",
        f"{packet.sprintf("%IP.proto%") if packet.haslayer('IP') and packet['IP'].proto else None}",
        f"{len(packet)}",
        f"{packet.summary()}"
    ])
    no += 1

    log_lines.append(line)
    if len(log_lines) > max_lines:
        log_lines.pop(0)

    # initial line and clear screen
    print("\033[2JNo | Time | Source | Destination | Protocol | Length | Summary")

    # packet log lines
    for i in range(len(log_lines)):
        print(log_lines[i])

    # separator
    print("-" * 100)

    # last packet summary
    if packet.haslayer('Ether'):
        print(f"Ethernet: {packet['Ether'].summary()}")
    else:
        print()
    if packet.haslayer('IP'):
        print(f"IP: {packet['IP'].summary()}")
    else:
        print() 
    if packet.haslayer('TCP'):
        print(f"TCP: {packet['TCP'].summary()}")
    elif packet.haslayer('UDP'):
        print(f"UDP: {packet['UDP'].summary()}")
    elif packet.haslayer('ICMP'):
        print(f"ICMP: {packet['ICMP'].summary()}")
    else:
        print()
    
    raw = bytes(packet).hex()
    if len(raw) > 100:
        print(f"{raw[:97]}...")
    else:
        print(raw)

