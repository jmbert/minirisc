#!/bin/sh

git submodule update --recursive

riscof --verbose info run --config ./config.ini --suite ./riscv-arch-test/riscv-test-suite/rv64i_m/ --env ./riscv-arch-test/riscv-test-suite/env/