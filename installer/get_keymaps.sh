#!/bin/sh
cd /run/current-system/profile/share/keymaps
keys=$(find ./ -type f)
echo "$keys" | grep -Po '/\K([^/]+)\.map\.gz$' | sed 's/\.map\.gz$//' | sort
