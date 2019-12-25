#!/bin/sh

stack exec clash -- --verilog grayCode.hs
sby -f grayCode.sby
