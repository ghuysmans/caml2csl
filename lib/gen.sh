#!/bin/sh
set -e
tool="$1"
shift
clib="-clib $1"
shift
while : ; do
	if [ -z "$1" ]; then
		break
	else
		$tool $clib $1
		shift
	fi
done
