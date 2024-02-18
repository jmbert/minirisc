#!/bin/sh

cmd=$1
if [ "$cmd" == "" ]; then
	cmd="run"
fi

riscof --verbose info $cmd --config ./config.ini --suite ./riscv-arch-test/riscv-test-suite/rv64i_m/ --env ./riscv-arch-test/riscv-test-suite/env/