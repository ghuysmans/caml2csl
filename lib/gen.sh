#!/bin/sh
clib="-clib $1"
shift
while : ; do
	if [ -z "$1" ]; then
		break
	else
		caml2csl $clib $1
		shift
	fi
done
