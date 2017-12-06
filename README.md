# Overview of Core65X2

This is a very simple MOS6502 core written in Rust, with the following features:

* All original 6502 opcodes.
* Passes all tests, including BCD operations from https://github.com/Klaus2m5/6502_65C02_functional_tests
* Provides a decoupled memory interface through trait, allowing MMU, memory-mapped I/O, etc.
* Provides handlers for signalling invalid opcode and software interrupts from external code.

The 'X' in the name is the intention of the author to expand the core emulation to other 6502-family members such as the GTE/Rockwell ones, the 65C02/65S02 from WDC, and the 65CE02/4510 used in the Commodore 65 prototypes.

# License

The MIT License
Copyright 2017 Hernán Di Pietro.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.





