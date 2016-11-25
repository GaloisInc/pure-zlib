A Pure ZLib
===========

This library is intended as a pure replacement for `zlib`, for systems in which
you either desire the benefits of a high-level implementation or for systems in
which `zlib` is not supported. Presently, the cost for these is decreased
performance; our buffering and output systems are not nearly as tuned as `zlib`s
has been, resulting in roughly 100x worse performance.

Getting to Know the Code
------------------------

Most users will want to use the functions in `Codec.Compression.Zlib`, which
provides both all-in-one as well as an incremental decompression functions. The
latter can be used in a stream of incremental parsers, or in situations in which
memory is limited.

Those that would like to hack on the implementation might look at the following
files:

  * [Codec.Compression.Zlib.Adler32](src/Codec/Compression/Zlib/Adler32.hs):
    Defines the Adler32 checksum function used in zlib `.Z` files.
  * [Codec.Compression.Zlib.Deflate](src/Codec/Compression/Zlib/Deflate.hs):
    Defines the core DEFLATE algorithm, as specified in [RFC
    1951](https://www.ietf.org/rfc/rfc1951.txt).
  * [Codec.Compression.Zlib.HuffmanTree](src/Codec/Compression/Zlib/HuffmanTree.hs):
    A simple implementation of a HuffmanTree.
  * [Codec.Compression.Zlib.Monad](src/Codec/Compression/Zlib/Monad.hs): A handy
    decompression monad used to run the thing. Based on a combined state and
    continuation monad.
  * [Codec.Compression.Zlib.OutputWindow](src/Codec/Compression/Zlib/OutputWindow.hs):
    The output window data structure, which is a combination of a
    [fingertree](https://hackage.haskell.org/package/fingertree) and a [ByteString
    builder](https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString-Builder.html).

Compression
-----------

Compression is not yet implemented by the library, but is [one of our early
TODOs](#5).

GZip Support
------------

The core DEFLATE implementation used in this library should be the same one used
in GZIP files. However, we currently only work with classic `*.Z` files, which
have a different header, output, and checksum. Expanding this library would
likely make it vastly more useful.

This is [another early TODO](#4).

Performance
-----------

Performance is notably worse that `zlib`, but probably still acceptable for many
uses. If you want to help improve the performance of `pure-zlib`, we are
certainly very, very excited for you to help. We've done a bit of work on it,
but the more the merrier.

What we have discovered is that most of the slowdown is in the `OutputWindow`
mechanism. We spend a lot of time in there keeping track of data and shifting
data out of the system. So ... this might be an obvious place to look.

We've done some considerable work on the input side, such that we don't think
that's necessarily a good place to mine. However, if you want to, and you can
show improvements, go for it.
