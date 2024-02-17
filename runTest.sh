#!/bin/bash
set -e

. /home/joe/src/minirisc/config.sh

rv64-elf2bin $1 $1-bin

# Get data from elf
endaddr=$(riscv64-unknown-elf-readelf -s $1 | grep write_tohost | printf "0x%X\n" $((0x$(awk '{print $2}') + 8)))
testdatabegin=0x$(riscv64-unknown-elf-readelf -s $1 |  grep begin_signature | awk '{print $2}')
testdataend=0x$(riscv64-unknown-elf-readelf -s $1 | grep end_signature | awk '{print $2}')

name=$(basename $(dirname $(dirname $(realpath $1))))

/home/joe/src/minirisc/zig-out/bin/minirisc $1-bin $2 $endaddr $testdatabegin $testdataend