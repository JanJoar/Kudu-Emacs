#!/bin/sh

#VARS
DISK=/dev/sda
HOME=/home/t/

sfdisk -f $DISK < part
parted -s $DISK resizepart 3 100%

parts=$(lsblk -o NAME -n -l)
BOOT_PART=/dev/$(echo "$parts" | awk 'NR==2{print $1}')
SWAP_PART=/dev/$(echo "$parts" | awk 'NR==3{print $1}')
ROOT_PART=/dev/$(echo "$parts" | awk 'NR==4{print $1}')

mkfs.fat -F32 $BOOT_PART
mkfs.ext4 $ROOT_PART
mkswap $SWAP_PART

swapon $SWAP_PART
mount $ROOT_PART /mnt

herd start cow-store /mnt

ROOT_FILESYSTEM=$(blkid -s UUID -o value $ROOT_PART)
sed "s/\$ROOT_FILESYSTEM/$ROOT_FILESYSTEM/g" config.scm.sed > config.scm

mkdir /mnt/etc
cp config.scm /mnt/etc/config.scm
guix system init /mnt/etc/config.scm /mnt

git clone https://github.com/JanJoar/Kudu-Emacs.git /mnt/$HOME/.emacs.d -b devel
