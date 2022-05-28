#!/bin/sh
set -xe
stack exec clash -- --verilog $1
vivado -mode batch -nojournal -source ../../fpga/compile.tcl
