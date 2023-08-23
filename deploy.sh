#!/usr/bin/env bash

if [ -z "$1" ]; then
	echo >&2 "usage: $0 TARGET_HOST"
	exit 1
fi

nixos-rebuild switch --flake ".#$1" --target-host "root@$1" --build-host localhost
