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

DISK=$disk
USERNAME=$username
HOSTNAME=$hostname
SCM_FILE=$(scm_file $iso)
TIMEZONE=$timezone
KEYMAP=$keymap
SWAP_UUID="swaps uuid"
ROOT_UUID="roots uuid"
scheme_template=$(cat $SCM_FILE)
scm=$(substitute_variables "$scheme_template" DISK HOSTNAME USERNAME SWAP_UUID ROOT_UUID TIMEZONE KEYMAP)
echo "$scm"
echo "Hostname: $hostname"
echo "Username: $username"
echo "Partition: $disk"
