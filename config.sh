#!/bin/sh


rv64-elf2bin() {
	riscv64-unknown-linux-objcopy -O binary -j .text -j .data -j .tohost -j .text.init $1 $2
} 