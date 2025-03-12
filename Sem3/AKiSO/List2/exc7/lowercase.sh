#!/bin/bash

if [ -z "$1" ]; then
	echo "Directory not specified. Try: $0 <path_to_directory>"
	exit 1
fi

for file in "$1"/*; do
	if [ -f "$file" ]; then
		name="${file##*/}"
		lower_name=$(echo "$name" | tr [A-Z] [a-z])
		if [ "$name" != "$lower_name" ]; then
			mv "$1/$name" "$1/$lower_name"
		fi
	fi
done
