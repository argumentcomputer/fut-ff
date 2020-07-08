# fut-ff

`fut-ff` is a [Futhark](https://futhark-lang.org/) program designed to be compiled to OpenCL code and run on a GPU. Its
goal is to implement generic finite field operations, but it has only been indirectly (through use in
[neptune-triton](https://github.com/filecoin-project/neptune-triton)) stress-tested with the arithmetic field of the
[BLS12-381 curve](https://electriccoin.co/blog/new-snark-curve/). Further work is needed to refine, optimize, and
provide exhaustive coverage for a general-purpose implementation. What *has* been implemented suffices for correct
Poseidon hashing as provided by [neptune](https://github.com/filecoin-project/neptune) and verified by GPU tests there.

## History

Initial design and algorithm details were partially based on (the source which has now become)
[ff-cl-gen](https://github.com/filecoin-project/ff-cl-gen).

`fut-ff` was written by [porcuquine](https://github.com/porcuquine).

## License

The Filecoin Project is dual-licensed under Apache 2.0 and MIT terms:

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
