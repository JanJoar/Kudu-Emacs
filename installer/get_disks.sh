#!/bin/sh
disks=$(lsblk -o NAME,TYPE -n -p -l | grep 'disk' | awk '{print $1}')
# Print each disk on a new line
echo "$disks"
