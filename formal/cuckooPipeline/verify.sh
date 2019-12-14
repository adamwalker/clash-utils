#!/bin/sh

stack exec clash -- --verilog cuckoo.hs
sby -f cuckoo.sby
