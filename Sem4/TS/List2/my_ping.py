from scapy.all import IP, ICMP, srloop
import time
import sys
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

# build package
packet = IP(dst=ip_address) / ICMP()

# initial message
print(f"MY_PING {target} ({ip_address}) {len(packet)}({len(packet[IP])}) bytes of data.")

# handle replies
def reply_callback(reply):
    ip = reply.src
    dns, _, _ = socket.gethostbyaddr(ip)
    print(f"{len(reply)} bytes from {dns} ({ip}): icmp_seq={reply[ICMP].seq} ttl={reply.ttl} time={reply.time:.2f}ms")


transmitted, received = 0, 0
start_time = time.time()
try:
    # send - receive 1 packet
    reply = srloop(packet, timeout=2, verbose=False, prn=reply_callback)
except KeyboardInterrupt:
    # todo statistics
    end_time = time.time()
    