#!/bin/sh

mode=$1
if [ "$mode" == "" ]; then
	mode="Debug"
fi

zig build -Doptimize=$mode