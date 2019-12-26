#!/bin/sh

stack exec clash -- --verilog firFilter.hs
sby -f firFilter.sby
