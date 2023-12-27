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
		--create-iso)
			iso=true
			shift
			;;
		--timezone)
			timezone="$2"
			shift
			;;
		--keymap)
			keymap="$2"
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
function scm_file() {
	iso=$1
	if [ "$iso" = true ]; then
		echo "guix_iso.scm"
		return
	fi
	echo "guix_config.scm"
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
	SCM_FILE=$4
	TIMEZONE=$5
	KEYMAP=$6


	part=$(get_parts $disk)
	swap_part=$(echo "$part" | awk 'NR==2{print $1}')
	root_part=$(echo "$part" | awk 'NR==3{print $1}')

	SWAP_UUID=$(get_part_uuid $swap_part)
	ROOT_UUID=$(get_part_uuid $root_part)

	scheme_template=$(cat $SCM_FILE)
	scm=$(substitute_variables "$scheme_template" DISK HOSTNAME USERNAME SWAP_UUID ROOT_UUID TIMEZONE KEYMAP)

	mkdir /mnt/etc
	echo "$scm" > /mnt/etc/config.scm
	guix pull
 	guix package -u
	hash guix
 	guix pull
 	guix package -u
	hash guix
	guix system init /mnt/etc/config.scm /mnt

	mkdir -p /mnt/home/$USERNAME/
	git clone https://github.com/JanJoar/Kudu-Emacs.git /mnt/$HOME/.emacs.d -b devel

}

make_disk $disk
install				\
	$disk			\
	$hostname		\
	$username		\
	$(scm_file $iso)	\
	$timezone		\
	$keymap