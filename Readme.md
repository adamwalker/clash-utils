# Clash utils

A random collection of reusable [Clash](http://www.clash-lang.org/) designs/examples.

## Includes:
* [Binary to BCD conversion](https://en.wikipedia.org/wiki/Double_dabble)
* [CORDIC](https://en.wikipedia.org/wiki/CORDIC) for calculating trigonometric functions
* [CRC](https://en.wikipedia.org/wiki/Cyclic_redundancy_check) calculation
* A FIFO
* [FIR](https://en.wikipedia.org/wiki/Finite_impulse_response), [IIR](https://en.wikipedia.org/wiki/Infinite_impulse_response) filters
* A [bitonic sorting network](https://en.wikipedia.org/wiki/Bitonic_sorter)
* Divider
* [FFTs](https://en.wikipedia.org/wiki/Fast_Fourier_transform)
* [Gray code](https://en.wikipedia.org/wiki/Gray_code) conversion
* [Hamming code](https://en.wikipedia.org/wiki/Hamming_code) encoding/decoding
* [LFSRs](https://en.wikipedia.org/wiki/Linear-feedback_shift_register)
* [Scrambler](https://en.wikipedia.org/wiki/Scrambler)
* [Pseudo LRU trees](https://en.wikipedia.org/wiki/Pseudo-LRU)
* The lookup side of a [Cuckoo hash table](https://en.wikipedia.org/wiki/Cuckoo_hashing)

Each design has a [Hspec](https://hspec.github.io/) testsuite/specification in the [/tests](https://github.com/adamwalker/clash-utils/tree/master/tests) subdirectory.

## Status

Most of these designs are not FPGA proven (yet). Designs that have actually been tested on an FPGA are:
* the CRCs
* the FIFOs

