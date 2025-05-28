#!/usr/bin/env python3

import pyudev
import os
import sys

def main(firmware):
    context = pyudev.Context()
    monitor = pyudev.Monitor.from_netlink(context)
    monitor.filter_by(subsystem='usb')
    monitor.start()

    for device in iter(monitor.poll, None):
        # I can add more logic here, to run different scripts for different
        # devices.
        print(device)
        # print("BUS: " + str(device.get("ID_BUS")))
        # print("DEVNAME: " + str(device.get("DEVNAME")))
        # print("ID_VENDOR_ID: " + str(device.get("ID_VENDOR_ID")))
        # print("ID_MODEL_ID: " + str(device.get("ID_MODEL_ID")))
        # print("BUSNUM: " + str(device.get("BUSNUM")))
        # print("DEVNUM: " + str(device.get("DEVNUM")))
        # print("is_init: " + str(device.is_initialized))
        # print("actions: " + str(device.action))
        print(device.keys())
        vid = device.get("ID_VENDOR_ID")
        pid = device.get("ID_MODEL_ID")
        action = device.action
        bus = str(device.get("BUSNUM"))
        addr = str(device.get("DEVNUM"))
                  
        if vid == "2e8a" and pid == "0003" and action == "bind":
            subprocess.call(['echo', 'lol'])
            os.system(f"picotool load -t uf2 {firmware} --bus {bus} --address {addr}  && ~/src/github/raspberrypi/picotool/build/picotool reboot &")

if __name__ == '__main__':
    if len(sys.argv) <= 1:
        print("No arguments were given")
    else:
        main(sys.argv[1])
