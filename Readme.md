# Clash utils

A collection of reusable [Clash](http://www.clash-lang.org/) designs/examples.

See [here](http://adamwalker.github.io/Building-FPGA-KVS/) and [here](http://adamwalker.github.io/Filter-Design-in-Clash/) for projects built using components in this library.

## Includes:
* [Binary to BCD conversion](https://en.wikipedia.org/wiki/Double_dabble), BCD arithmetic that efficiently utilises the carry chain.
* A [bitonic sorting network](https://en.wikipedia.org/wiki/Bitonic_sorter)
* [CORDIC](https://en.wikipedia.org/wiki/CORDIC) for calculating trigonometric functions
* [CRC](https://en.wikipedia.org/wiki/Cyclic_redundancy_check) calculation
* A [Cuckoo hash table](https://en.wikipedia.org/wiki/Cuckoo_hashing)
* Divider
* [FFTs](https://en.wikipedia.org/wiki/Fast_Fourier_transform)
* A FIFO
* [FIR](https://en.wikipedia.org/wiki/Finite_impulse_response), [IIR](https://en.wikipedia.org/wiki/Infinite_impulse_response) and [CIC](https://en.wikipedia.org/wiki/Cascaded_integrator%E2%80%93comb_filter) filters
* [Gray code](https://en.wikipedia.org/wiki/Gray_code) conversion
* [Hamming code](https://en.wikipedia.org/wiki/Hamming_code) encoding/decoding
* [LFSRs](https://en.wikipedia.org/wiki/Linear-feedback_shift_register)
* [Multi port block rams](http://www.eecg.toronto.edu/~steffan/papers/laforest_xor_fpga12.pdf)
* Multipliers
* [Prefix sums](https://en.wikipedia.org/wiki/Prefix_sum), [Carry lookahead adders](https://en.wikipedia.org/wiki/Carry-lookahead_adder), [Carry select adders](https://en.wikipedia.org/wiki/Carry-select_adder) and [Carry save adders](https://en.wikipedia.org/wiki/Carry-save_adder)
* [Pseudo LRU trees](https://en.wikipedia.org/wiki/Pseudo-LRU)
* Regular expression matching
* [Scrambler](https://en.wikipedia.org/wiki/Scrambler)
* [SHA3](https://en.wikipedia.org/wiki/SHA-3) and [AES](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard)
* Utilities for streaming data

Each design has a [Hspec](https://hspec.github.io/) testsuite/specification in the [/tests](https://github.com/adamwalker/clash-utils/tree/master/tests) subdirectory. A handful of designs have [formal properties](https://symbiyosys.readthedocs.io/en/latest/) in the [/formal](https://github.com/adamwalker/clash-utils/tree/master/formal) directory.

## Status

FPGA proven designs are indicated in the module documentation.
