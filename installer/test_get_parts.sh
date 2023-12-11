#!/bin/bash


function test_part() {

	disk=$1
	part=$(lsblk -o NAME,TYPE -n -p -l | awk -v disk="$disk" '$2=="part" && index($1, disk)==1 {print $1}')
	echo "$part"
}
