# 128-bit Integer Types

* Proposal: [SE-NNNN](nnnn-int128.md)
* Author: [Stephen Canon](https://github.com/stephentyrone)
* Review Manager: [Doug Gregor](https://github.com/DougGregor)
* Implementation: https://github.com/stephentyrone/swift-numerics/tree/int128
* Status: **Awaiting review**

## Motivation

128b integers are the largest fixed-size type that is currently commonly
used in "general-purpose" code. They are much less common than 64b types,
but common enough that adding them to the standard library makes sense.
We use them internally in the standard library already (e.g. as an 
implementation detail of Duration).

## Proposed solution

Introduce two new structs, `UInt128` and `Int128`, conforming to all of the
usual fixed-width integer protocols.

While these will be implemented in the standard library, for the purposes of
experimentation, I have put them on a branch of swift-numerics. To try these
types out, use my branch `int128`:
```swift
.package(
  url: "https://github.com/stephentyrone/swift-numerics",
  branch: "int128"
)
```
and add `Int128Demo` as a dependency:
```swift
.target(name: "MyTarget", dependencies: [
  .product(name: "Int128Demo", package: "swift-numerics")
]),
```
This branch requires a recent nightly toolchain to build.

## Detailed design

The `[U]Int128` types are 16B aligned on 64b targets¹ and have the same
alignment as `[U]Int64` on 32b targets. They will match the endianness of
all other integer types.

The clang importer will be updated to bridge `__uint128_t` to `UInt128` and
`__int128_t` to `Int128`. We will not bridge `_BitInt()` types until
the ABI problems with those types have been clearly resolved (see Alternatives
Considered for sordid history).

The `[U]Int128` types conform to `AtomicRepresentable` on targets with
`_hasAtomicBitWidth(_128)` set (notably x86\_64, arm64, and arm64\_32).

The `[U]Int128` types conform to `Codable`; however, many existing encoders
and decoders do not support such large integer types. Therefore they are
encoded as a pair of 64b integers. This pair is always in little-endian
order, regardless of the endianness of the architecture.

-------
¹ For the purposes of this discussion, arm64\_32 and similar architectures
are "64b targets."

## Source compatibility

This proposal has no effect on source compatibility.

## ABI compatibility

This proposal has no effect on ABI compatibility.

## Implications on adoption

Adopting this feature will require a target with runtime support.

## Future directions

Implement clang importer support for `_BitInt(128)` on any platforms where
the finalized ABI is compatible with our layout.

## Alternatives considered

### Alignment and `_BitInt()` types
Clang and GCC have historically exposed the extension types `__uint128_t` and
`__int128_t` on 64b platforms only. These types basically behave like C
builtin integer types--their size and alignment are 16B.

The C23 standard introduces `_BitInt(N)` as a means to spell arbitrary-width
integer types, but these still have some warts. In particular, `_BitInt(128)`
as implemented in clang has 8B alignment on x86\_64 and arm64. For arm64,
this is clearly a bug; the AAPCS specifies that it should have 16B alignment.
For x86\_64, the situation is less clear. The x86\_64 psABI document specifies
that it should have 8B alignment, but the authors of the proposal that added
the feature tell me that it _should_ be 16B aligned and that they are 
attempting to change the psABI.

We would like to be layout-compatible with `_BitInt(128)` on all platforms,
but given the currently-murky state of the layout of those types, it makes
the most sense to guarantee compatibility with the widely-used but non-
standard `__[u]int128_t` and find mechanisms to make `_BitInt(128)` work
once its ABI has been finalized on Swift's targeted platforms.

### Generic-sized fixed width integers
Rather than adding `[U]Int128`, we could implement some form of generic-
sized fixed-width integer (like `\_BitInt()` in C). Given both the lack
of consensus around what integer generic parameters ought to look like in
Swift (or if they ought to exist at all), and the growing pains that 
`\_BitInt()` is currently going through, such a design would be premature.
While other fixed-width integer types are interesting, 128 bits is a couple
orders of magnitude more useful than all the others for general-purpose
software at this point in time.

### Codable conformance
Because many existing encoders and decoders do not already support 128b
integers, we use a pair of 64b integers instead. To maximize compatibility,
or to improve human-readability, we could instead encode these types as
decimal or hexadecimal strings. However, this would be somewhat less 
efficient in some use cases, and it is possible for users to achieve the
same effect by converting to a string before encoding.
