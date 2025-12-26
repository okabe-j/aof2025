#!/usr/bin/env python3

import serial
import sys
import time

PORT = sys.argv[1] if len(sys.argv) > 1 else "/dev/tty.usbmodem14202"
BAUD = int(sys.argv[2]) if len(sys.argv) > 2 else 100000

def hexdump(data, offset):
    for i in range(0, len(data), 16):
        chunk = data[i:i+16]
        hex_part = " ".join(f"{b:02x}" for b in chunk)
        ascii_part = "".join(chr(b) if 32 <= b < 127 else "." for b in chunk)
        print(f"{offset+i:08x}  {hex_part:<47}  |{ascii_part}|")


testcase = (open("/Users/jiamingzhao/Documents/hardcaml/aof2025/test/testcase/day01.txt").read() + "\x04").encode("ascii")

def main():
    ser = serial.Serial(
        port=PORT,
        baudrate=BAUD,
        timeout=1
    )

    print(f"Listening on {PORT} @ {BAUD} baud (Ctrl-C to quit)")
    print("Waiting 10 seconds to start...")
    time.sleep(10)
    offset = 0

    try:
        ser.write(testcase)
        while True:
            data = ser.read(256)
            if data:
                hexdump(data, offset)
                offset += len(data)
                time.sleep(0.1)
    except KeyboardInterrupt:
        print("\nExiting.")
    finally:
        ser.close()

if __name__ == "__main__":
    main()
