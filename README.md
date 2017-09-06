# Data.Bitstream

*This is an alternative implementation to the `[Bool]` heavy solution in
[Data.BitCode](https://github.com/angerman/data-bitcode).*

`Data.Bitstream` is a LLVM bitcode serializer. The module takes bitcode
records and turns them into the bitcode bitstream, which is then written
to disk as bytes.

## Bitcode

LLVM Bitcode is a series of bits emitted into a stream. This does not 
need to align with bytes. Notably bytes on disk usually store their least
significant bit last, while it would come first in bitcode. Example:

| byte           | `de`       | `c0`      | `17`       | `0b`       |
| bits on disk   | `11011110` | `1100000` | `00010111` | `00001011` |
| bits in stream | `01111011` | `0000011` | `11101000` | `00001011` |

However flipping the bytes in the `ByteString` prior to writing to disk
seems rather cheap (~16M/s) (see `bench.html`).

## Building 

With [Data.BitCode](https://github.com/angerman/data-bitcode) and
[Data.BitCode.LLVM](https://gihtub.com/angerman/data-bitcode-llvm), checked
out in the same directory as
[Data.Bistream](https://github.com/angerman/data-bitstream), this module should
be buildable with `cabal new-build` (see also the `cabal.project` file).

## Testing

`cabal new-test` should run a series of tests, trying to produce bitcode and
ensure some basic validity of the `Bitstream`.

## Benchmarking

`cabal new-bench` should try to do some rather expensive, yet common bit
concatinations on `Bitstream` and the underlying `Stream` data types. The
benchmark has two parts. One is a synthetic which tries some simple 
concatination. And the other one is a real world benchmark which attempts
to build the object file for `main = putStrLn 'Hello World'` by taking the
serialized bitcode records for said program and runs them through the
serializer.

## Profiling

The module also contains the `test` executable. Which takes a serialized
bitcode record file as input and runs the `Bitstream` serializer over it.
`cabal new-build --enable-profiling` should produce the profiled binary.

```
$(find dist-newstyle -name "test" -type f) bench/data/Main.bcbin +RTS -P
```

should produce the `test.prof` profile.

*I like [viewprof](https://hackage.haskell.org/package/viewprof) a lot for
viewing haskell profiles*
