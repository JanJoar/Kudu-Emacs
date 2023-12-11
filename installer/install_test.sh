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
		--partition)
			partition="$2"
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

DISK=$disk
USERNAME=$username
HOSTNAME=$hostname
SWAP_UUID="swaps uuid"
ROOT_UUID="roots uuid"
scheme_template=$(cat guix_config.scm)
scm=$(substitute_variables "$scheme_template" DISK HOSTNAME USERNAME SWAP_UUID ROOT_UUID)
echo "$scm"
echo "Hostname: $hostname"
echo "Username: $username"
echo "Partition: $partition"
