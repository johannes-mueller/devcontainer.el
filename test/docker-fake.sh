#!/bin/bash

case $1 in
    "null-result")
	echo ""
	;;
    "error")
	exit 1
	;;
    "one-line")
	echo "$2"
	;;
esac
