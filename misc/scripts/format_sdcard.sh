#!/bin/bash

# This script creates a FAT disk image containing the directory tree for Wee
# Noise Maker samples.

device=$1

if [ -z "$device" ]
then
    echo "Usage: $0 /dev/<SDCARD_DEVICE>"
    exit
fi

dialog --title "Are you sure?"  --yesno "You are about to format the sdcard:\
$device.\nAre you sure?" 10 50

if [ "$?" != "0" ]
then
    exit
fi

echo "Let's go!!!"

echo parted $device

sudo parted --script $device \
     mklabel msdos \
     mkpart primary fat32 1MiB 100% \
     set 1 boot on \
     quit

echo mkfs.vfat "$device"p1

sudo mkfs.vfat -F 32 "$device"p1

echo Mounting "$device"p1

sudo mount "$device"p1 /mnt

sudo mkdir -p /mnt/samples/drums/kick
sudo mkdir -p /mnt/samples/drums/snare
sudo mkdir -p /mnt/samples/drums/tom
sudo mkdir -p /mnt/samples/drums/cymbal
sudo mkdir -p /mnt/samples/drums/hat
sudo mkdir -p /mnt/samples/drums/clap
sudo mkdir -p /mnt/samples/drums/misc
sudo mkdir -p /mnt/samples/vocals
sudo mkdir -p /mnt/samples/misc

sudo cp -r ~/Music/WNM/samples/ /mnt/

tree /mnt/

sudo umount /mnt

