#!/bin/bash

while [[ "$#" -gt 0 ]]; do
	case $1 in
		--hostname)
			hostname="$2"
			shift
			;;
		--username)
			username="$2"
			shift
			;;
		--disk)
			disk="$2"
			shift
			;;
		*)
			echo "Unknown option: $1"
			exit 1
			;;
	esac
	shift
done

function get_parts() {
	disk=$1
	part=$(lsblk -o NAME,TYPE -n -p -l | awk -v disk="$disk" '$2=="part"')
	echo "$part"
}
function substitute_variables() {
	local str="$1"
	shift
	for var; do
	str="${str//\$$var/${!var}}"
	done
	echo "$str"
}
function make_disk() {
	disk=$1
	sfdisk -f $disk < part.sfdisk
	parted -s $disk resizepart 3 100%
	part=$(get_parts $disk)
	BOOT_PART=$(echo "$part" | awk 'NR==1{print $1}')
	SWAP_PART=$(echo "$part" | awk 'NR==2{print $1}')
	ROOT_PART=$(echo "$part" | awk 'NR==3{print $1}')

	mkfs.fat -F32 $BOOT_PART
	mkfs.ext4 -F $ROOT_PART
	mkswap $SWAP_PART

	swapon $SWAP_PART
	mount $ROOT_PART /mnt
	herd start cow-store /mnt
}
function get_part_uuid() {
	part=$1
	blkid -s UUID -o value $part
}
function install() {
	DISK=$1
	HOSTNAME=$2
	USERNAME=$3

	part=$(get_parts $disk)
	swap_part=$(echo "$part" | awk 'NR==2{print $1}')
	root_part=$(echo "$part" | awk 'NR==3{print $1}')

	SWAP_UUID=$(get_part_uuid $swap_part)
	ROOT_UUID=$(get_part_uuid $root_part)

	scheme_template=$(cat guix_config.scm)
	scm=$(substitute_variables "$scheme_template" DISK HOSTNAME USERNAME SWAP_UUID ROOT_UUID)

	mkdir /mnt/etc
	echo "$scm" > /mnt/etc/config.scm
	guix system init /mnt/etc/config.scm /mnt

	git clone https://github.com/JanJoar/Kudu-Emacs.git /mnt/$HOME/.emacs.d -b devel

}

make_disk $disk
install $disk $hostname $username
