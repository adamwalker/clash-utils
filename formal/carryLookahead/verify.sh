#!/bin/sh

stack exec clash -- --verilog koggeStone.hs
sby -f koggeStone.sby
