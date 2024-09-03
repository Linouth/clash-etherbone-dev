import socket


sock = socket.socket(socket.AF_INET, socket.SOCK_RAW, socket.IPPROTO_UDP)
sock.bind(('127.0.0.1', 0x1111))

while True:
    packet, addr = sock.recvfrom(0xffff)
    print(packet.hex())
