from scapy.all import IP, ICMP, sr1
import time
import sys
import signal
import socket

# check args
if len(sys.argv) != 2:
    print("Usage: python3 my_ping.py <target>")
    sys.exit(1)

# get target dns / address
target = sys.argv[1]

# resolve target ip address
try:
    ip_address = socket.gethostbyname(target)
except socket.gaierror:
    print(f"Error: Unable to resolve hostname {target}")
    sys.exit(1)

transmitted, received = 0, 0
start_time = time.time()
rtt_min, rtt_max, rtt_avg = float('inf'), 0, 0

# handle replies
def reply_callback(sent, reply):
    global received
    received += 1

    global rtt_min, rtt_max, rtt_avg
    rtt = (reply.time - sent.sent_time) * 1000
    rtt_min = min(rtt_min, rtt)
    rtt_max = max(rtt_max, rtt)
    rtt_avg += rtt

    ip = reply.src
    dns, _, _ = socket.gethostbyaddr(ip)

    print(f"{len(reply)} bytes from {dns} ({ip}): icmp_seq={reply[ICMP].seq} ttl={reply.ttl} time={rtt:.1f}ms")

def sigint_handler(signal, frame):
    global transmitted, received, start_time, rtt_min, rtt_avg, rtt_max

    total_time = 1000 * (time.time() - start_time)
    percentage_loss = (1 - received / transmitted) * 100 if transmitted > 0 else 0
    rtt_min, rtt_avg = [rtt_min, rtt_avg / received] if received > 0 else [0, 0]

    print(f"\n--- {target} ping statistics ---")
    print(f"{transmitted} packets transmitted, {received} received, {percentage_loss}% packet loss, time {total_time:.0f}ms")
    print(f"rtt min/avg/max = {rtt_min:.1f}/{rtt_avg:.1f}/{rtt_max:.1f} ms")
    sys.exit(0)

signal.signal(signal.SIGINT, sigint_handler)


# build package
packet = IP(dst=ip_address) / ICMP()

# initial message
print(f"MY_PING {target} ({ip_address}) {len(packet)}({len(packet[IP])}) bytes of data.")

# send - receive in loop
while True:
    time.sleep(1)
    transmitted += 1
    reply = sr1(packet, timeout=5, verbose=0)
    
    if reply is None:
        print(f"Request timed out for {target}")
        sys.exit(1)
    else:
        reply_callback(packet, reply)