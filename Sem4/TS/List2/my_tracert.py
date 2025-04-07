from scapy.all import IP, ICMP, Raw, sr1
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

max_hops = 30
package_size = 60

# initial message
print(f"MyTraceroute to {target} ({ip_address}), {max_hops} hops max, {package_size} bytes packets")

packet = IP(dst=ip_address) / ICMP() / Raw(b'A' * (package_size - 28))
for ttl in range(1, max_hops + 1):
    packet[IP].ttl = ttl
    reply = sr1(packet, timeout=2, verbose=0)
    
    if reply is None:
        print(f"{ttl}: * * *")
    else:
        ip = reply.src

        try:
            dns, _, _ = socket.gethostbyaddr(ip)
        except socket.herror:
            dns = ip
        
        rtt = (reply.time - packet.sent_time) * 1000
        print(f"{ttl}: {dns} ({ip}) {rtt:.3f} ms")

        # echo reply type - destination reached
        if reply.type == 0:
            break