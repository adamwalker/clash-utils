#!/bin/sh

stack exec clash -- --verilog multiPortBlockRam.hs
sby -f multiPortBlockRam.sby
