#!/bin/sh
set -xe
stack exec clash -- -fclash-aggressive-x-optimization-blackboxes --verilog $1
vivado -mode batch -nojournal -source ../../fpga/compile.tcl
