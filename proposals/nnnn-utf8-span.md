# Safe Access to Contiguous UTF-8 Storage

* Proposal: [SE-NNNN](nnnn-utf8-span.md)
* Authors: [Michael Ilseman](https://github.com/milseman), [Guillaume Lessard](https://github.com/glessard)
* Review Manager: TBD
* Status: **Awaiting implementation**
* Bug: rdar://48132971, rdar://96837923
* Implementation: (pending)
* Upcoming Feature Flag: (pending)
* Review: ([pitch 1](https://forums.swift.org/t/pitch-utf-8-processing-over-unsafe-contiguous-bytes/69715))

## Introduction

We introduce `UTF8Span` for efficient and safe Unicode processing over contiguous storage.

Native `String`s are stored as validly-encoded UTF-8 bytes in an internal contiguous memory buffer. The standard library implements `String`'s API as internal methods which operate on top of this buffer, taking advantage of the validly-encoded invariant and specialized Unicode knowledge. We propose making this UTF-8 buffer and its methods public as API for more advanced libraries and developers.

## Motivation

Currently, if a developer wants to do `String`-like processing over UTF-8 bytes, they have to make an instance of `String`, which allocates a native storage class and copies all the bytes. The developer would then need to operate within the new `String`'s views and map between `String.Index` and byte offsets in the original buffer.

For example, if these bytes were part of a data structure, the developer would need to decide to either cache such a new `String` instance or recreate it on the fly. Caching more than doubles the size and adds caching complexity. Recreating it on the fly adds a linear time factor and class instance allocation/deallocation.

Furthermore, `String` may not be available on all embedded platforms due to the fact that it's conformance to `Comparable` and `Collection` depend on data tables bundled with the stdlib. `UTF8Span` is a more appropriate type for these platforms, and only some explicit API make use of data tables.



### UTF-8 validity and efficiency

UTF-8 validation is particularly common concern and the subject of a fair amount of [research](https://lemire.me/blog/2020/10/20/ridiculously-fast-unicode-utf-8-validation/). Once an input is known to be validly encoded UTF-8, subsequent operations such as decoding, grapheme breaking, comparison, etc., can be implemented much more efficiently under this assumption of validity. Swift's `String` type's native storage is guaranteed-valid-UTF8 for this reason.

Failure to guarantee UTF-8 encoding validity creates security and safety concerns. With invalidly-encoded contents, memory safety would become more nuanced. An ill-formed leading byte can dictate a scalar length that is longer than the memory buffer. The buffer may have bounds associated with it, which differs from the bounds dictated by its contents.

Additionally, a particular scalar value in valid UTF-8 has only one encoding, but invalid UTF-8 could have the same value encoded as an [overlong encoding](https://en.wikipedia.org/wiki/UTF-8#Overlong_encodings), which would compromise code that checks for the presence of a scalar value by looking at the encoded bytes (or that does a byte-wise comparison).


## Proposed solution

We propose a non-escapable `UTF8Span` which exposes a similar API surface as `String` for validly-encoded UTF-8 code units in contiguous memory.


## Detailed design

`UTF8Span` is a borrowed view into contiguous memory containing validly-encoded UTF-8 code units.

```swift
@frozen
public struct UTF8Span: Copyable, ~Escapable {
  @usableFromInline
  internal var _start: Index

  /*
   A bit-packed count and flags (such as isASCII)

   ┌───────┬──────────┬───────┐
   │ b63   │ b62:56   │ b56:0 │
   ├───────┼──────────┼───────┤
   │ ASCII │ reserved │ count │
   └───────┴──────────┴───────┘

   Future bits could be used for all <0x300 scalar (aka <0xC0 byte)
   flag which denotes the quickest NFC check, a quickCheck NFC
   flag (using Unicode data tables), a full-check NFC flag,
   single-scalar-grapheme-clusters flag, etc.

   */
  @usableFromInline
  internal var _countAndFlags: UInt64
}
```

### Creation and validation

`UTF8Span` is validated at initialization time, and encoding errors are diagnosed and thrown.

```swift
extension Unicode.UTF8 {
  /// The kind of encoding error encountered during validation
  @frozen
  public struct EncodingErrorKind: Error, Sendable, Hashable, Codable {
    public var rawValue: UInt8

    @inlinable
    public init(rawValue: UInt8)

    @_alwaysEmitIntoClient
    public static var unexpectedContinuationByte: Self { get }

    @_alwaysEmitIntoClient
    public static var overlongEncoding: Self { get }

    @_alwaysEmitIntoClient
    public static var invalidCodePoint: Self { get }
  }
}
```

**TODO**: Check all the kinds of errors we'd like to diagnose. Since this is a `RawRepresentable` struct, we can still extend it with a (finite) number of error kinds in the future.

```swift
extension UTF8Span {
  /// The kind and location of invalidly-encoded UTF-8 bytes
  @frozen
  public struct EncodingError: Error, Sendable, Hashable, Codable {
    /// The kind of encoding error
    public var kind: Unicode.UTF8.EncodingErrorKind

    /// The range of offsets into our input containing the error
    public var range: Range<Int>
  }

  public init(
    validating codeUnits: Span<UInt8>
  ) throws(EncodingError) -> dependsOn(codeUnits) Self

  public init<Owner: ~Copyable & ~Escapable>(
    nulTerminatedCString: UnsafeRawPointer,
    owner: borrowing Owner
  ) throws(EncodingError) -> dependsOn(owner) Self

  public init<Owner: ~Copyable & ~Escapable>(
    nulTerminatedCString: UnsafePointer<CChar>,
    owner: borrowing Owner
  ) throws(EncodingError) -> dependsOn(owner) Self
}
```

### Views

Similarly to `String`, `UTF8Span` exposes different ways to view the UTF-8 contents.

`UTF8Span.UnicodeScalarView` corresponds to `String.UnicodeScalarView` for read-only purposes, however it is not `RangeReplaceable` as `UTF8Span` provides read-only access. Similarly, `UTF8Span.CharacterView` corresponds to `String`'s character view (i.e. its default view), `UTF8Span.UTF16View` to `String.UTF16View`, and `UTF8Span.CodeUnits` to `String.UTF8View`.

```swift
extension UTF8Span {
  public typealias CodeUnits = Span<UInt8>

  @inlinable
  public var codeUnits: CodeUnits { get }

  @frozen
  public struct UnicodeScalarView: ~Escapable {
    public let span: UTF8Span

    @inlinable
    public init(_ span: UTF8Span)
  }

  @inlinable
  public var unicodeScalars: UnicodeScalarView { _read }

  @frozen
  public struct CharacterView: ~Escapable {
    public let span: UTF8Span

    @inlinable
    public init(_ span: UTF8Span)
  }

  @inlinable
  public var characters: CharacterView { _read }

  @frozen
  public struct UTF16View: ~Escapable {
    public let span: UTF8Span

    @inlinable
    public init(_ span: UTF8Span)
  }

  @inlinable
  public var utf16: UTF16View { _read }
}
```

**TOOD**: `_read` vs `get`? `@inlinable` vs `@_alwaysEmitIntoClient`?

##### `Collection`-like API:

Like `Span`, `UTF8Span` provides index and `Collection`-like API:


```swift
extension UTF8Span {
  public typealias Index = RawSpan.Index
}

extension UTF8Span.UnicodeScalarView {
  @frozen
  public struct Index: Comparable, Hashable {
    public var position: UTF8Span.Index

    @inlinable
    public init(_ position: UTF8Span.Index)

    @inlinable
    public static func < (
      lhs: UTF8Span.UnicodeScalarView.Index,
      rhs: UTF8Span.UnicodeScalarView.Index
    ) -> Bool
  }

  public typealias Element = Unicode.Scalar

  @frozen
  public struct Iterator: ~Escapable {
    public typealias Element = Unicode.Scalar

    public let span: UTF8Span

    public var position: UTF8Span.Index

    @inlinable
    init(_ span: UTF8Span)

    @inlinable
    public mutating func next() -> Unicode.Scalar?
  }

  @inlinable
  public borrowing func makeIterator() -> Iterator

  @inlinable
  public var startIndex: Index { get }

  @inlinable
  public var endIndex: Index { get }

  @inlinable
  public var count: Int { get }

  @inlinable
  public var isEmpty: Bool { get }

  @inlinable
  public var indices: Range<Index> { get }

  @inlinable
  public func index(after i: Index) -> Index

  @inlinable
  public func index(before i: Index) -> Index

  @inlinable
  public func index(
    _ i: Index, offsetBy distance: Int, limitedBy limit: Index
  ) -> Index?

  @inlinable
  public func formIndex(after i: inout Index)

  @inlinable
  public func formIndex(before i: inout Index)

  @inlinable
  public func index(_ i: Index, offsetBy distance: Int) -> Index

  @inlinable
  public func formIndex(_ i: inout Index, offsetBy distance: Int)

  @inlinable
  public func formIndex(
    _ i: inout Index, offsetBy distance: Int, limitedBy limit: Index
  ) -> Bool

  @inlinable
  public subscript(position: Index) -> Element { borrowing _read }

  @inlinable
  public subscript(unchecked position: Index) -> Element { 
    borrowing _read
  }

  @inlinable
  public subscript(bounds: Range<Index>) -> Self { get }

  @inlinable
  public subscript(unchecked bounds: Range<Index>) -> Self {
    borrowing get
  }

  @_alwaysEmitIntoClient
  public subscript(bounds: some RangeExpression<Index>) -> Self {
    borrowing get
  }

  @_alwaysEmitIntoClient
  public subscript(
    unchecked bounds: some RangeExpression<Index>
  ) -> Self {
    borrowing get
  }

  @_alwaysEmitIntoClient
  public subscript(x: UnboundedRange) -> Self {
    borrowing get
  }

  @inlinable
  public func distance(from start: Index, to end: Index) -> Int

  @inlinable
  public func elementsEqual(_ other: Self) -> Bool

  @inlinable
  public func elementsEqual(_ other: some Sequence<Element>) -> Bool
}

extension UTF8Span.CharacterView {
  @frozen
  public struct Index: Comparable, Hashable {
    public var position: UTF8Span.Index

    @inlinable
    public init(_ position: UTF8Span.Index)

    @inlinable
    public static func < (
      lhs: UTF8Span.CharacterView.Index,
      rhs: UTF8Span.CharacterView.Index
    ) -> Bool
  }

  public typealias Element = Character

  @frozen
  public struct Iterator: ~Escapable {
    public typealias Element = Character

    public let span: UTF8Span

    public var position: UTF8Span.Index

    @inlinable
    init(_ span: UTF8Span)

    @inlinable
    public mutating func next() -> Character?
  }

  @inlinable
  public borrowing func makeIterator() -> Iterator

  @inlinable
  public var startIndex: Index { get }

  @inlinable
  public var endIndex: Index { get }

  @inlinable
  public var count: Int { get }

  @inlinable
  public var isEmpty: Bool { get }

  @inlinable
  public var indices: Range<Index> { get }

  @inlinable
  public func index(after i: Index) -> Index

  @inlinable
  public func index(before i: Index) -> Index

  @inlinable
  public func index(
    _ i: Index, offsetBy distance: Int, limitedBy limit: Index
  ) -> Index?

  @inlinable
  public func formIndex(after i: inout Index)

  @inlinable
  public func formIndex(before i: inout Index)

  @inlinable
  public func index(_ i: Index, offsetBy distance: Int) -> Index

  @inlinable
  public func formIndex(_ i: inout Index, offsetBy distance: Int)

  @inlinable
  public func formIndex(
    _ i: inout Index, offsetBy distance: Int, limitedBy limit: Index
  ) -> Bool

  @inlinable
  public subscript(position: Index) -> Element { borrowing _read }

  @inlinable
  public subscript(unchecked position: Index) -> Element { 
    borrowing _read
  }

  @inlinable
  public subscript(bounds: Range<Index>) -> Self { get }

  @inlinable
  public subscript(unchecked bounds: Range<Index>) -> Self {
    borrowing get
  }

  @_alwaysEmitIntoClient
  public subscript(bounds: some RangeExpression<Index>) -> Self {
    borrowing get
  }

  @_alwaysEmitIntoClient
  public subscript(
    unchecked bounds: some RangeExpression<Index>
  ) -> Self {
    borrowing get
  }

  @_alwaysEmitIntoClient
  public subscript(x: UnboundedRange) -> Self {
    borrowing get
  }

  @inlinable
  public func distance(from start: Index, to end: Index) -> Int

  @inlinable
  public func elementsEqual(_ other: Self) -> Bool

  @inlinable
  public func elementsEqual(_ other: some Sequence<Element>) -> Bool
}

extension UTF8Span.UTF16View {
  @frozen
  public struct Index: Comparable, Hashable {
    @usableFromInline
    internal var _rawValue: UInt64

    @inlinable
    public var position: UTF8Span.Index { get }

    /// Whether this index is referring to the second code unit of a non-BMP
    /// Unicode Scalar value.
    @inlinable
    public var secondCodeUnit: Bool { get }

    @inlinable
    public init(_ position: UTF8Span.Index, secondCodeUnit: Bool)

    @inlinable
    public static func < (
      lhs: UTF8Span.UTF16View.Index,
      rhs: UTF8Span.UTF16View.Index
    ) -> Bool
  }

  public typealias Element = UInt16

  @frozen
  public struct Iterator: ~Escapable {
    public typealias Element = UInt16

    public let span: UTF8Span

    public var index: UTF8Span.UTF16View.Index

    @inlinable
    init(_ span: UTF8Span)

    @inlinable
    public mutating func next() -> UInt16?
  }

  @inlinable
  public borrowing func makeIterator() -> Iterator

  @inlinable
  public var startIndex: Index { get }

  @inlinable
  public var endIndex: Index { get }

  @inlinable
  public var count: Int { get }

  @inlinable
  public var isEmpty: Bool { get }

  @inlinable
  public var indices: Range<Index> { get }

  @inlinable
  public func index(after i: Index) -> Index

  @inlinable
  public func index(before i: Index) -> Index

  @inlinable
  public func index(
    _ i: Index, offsetBy distance: Int, limitedBy limit: Index
  ) -> Index?

  @inlinable
  public func formIndex(after i: inout Index)

  @inlinable
  public func formIndex(before i: inout Index)

  @inlinable
  public func index(_ i: Index, offsetBy distance: Int) -> Index

  @inlinable
  public func formIndex(_ i: inout Index, offsetBy distance: Int)

  @inlinable
  public func formIndex(
    _ i: inout Index, offsetBy distance: Int, limitedBy limit: Index
  ) -> Bool

  @inlinable
  public subscript(position: Index) -> Element { borrowing _read }

  @inlinable
  public subscript(unchecked position: Index) -> Element { 
    borrowing _read
  }

  @inlinable
  public subscript(bounds: Range<Index>) -> Self { get }

  @inlinable
  public subscript(unchecked bounds: Range<Index>) -> Self {
    borrowing get
  }

  @_alwaysEmitIntoClient
  public subscript(bounds: some RangeExpression<Index>) -> Self {
    borrowing get
  }

  @_alwaysEmitIntoClient
  public subscript(
    unchecked bounds: some RangeExpression<Index>
  ) -> Self {
    borrowing get
  }

  @_alwaysEmitIntoClient
  public subscript(x: UnboundedRange) -> Self {
    borrowing get
  }

  @inlinable
  public func distance(from start: Index, to end: Index) -> Int

  @inlinable
  public func elementsEqual(_ other: Self) -> Bool

  @inlinable
  public func elementsEqual(_ other: some Sequence<Element>) -> Bool
}
```

### Queries

```swift
extension UTF8Span {
  /// Returns whether the validated contents were all-ASCII. This is checked at
  /// initialization time and remembered.
  @inlinable
  public var isASCII: Bool { get }

  /// Whether `i` is on a boundary between Unicode scalar values
  @inlinable
  public func isScalarAligned(_ i: UTF8Span.Index) -> Bool

  /// Whether `i` is on a boundary between `Character`s, i.e. extended grapheme clusters.
  @inlinable
  public func isCharacterAligned(_ i: UTF8Span.Index) -> Bool

  /// Whether `self` is equivalent to `other` under Unicode Canonical Equivalance
  public func isCanonicallyEquivalent(to other: UTF8Span) -> Bool

  /// Whether `self` orders less than `other` under Unicode Canonical Equivalance
  /// using normalized code-unit order
  public func isCanonicallyLessThan(_ other: UTF8Span) -> Bool
}
```

### Additions to `String` and `RawSpan`

We extend `String` with the ability to access its backing `UTF8Span`:

```swift
extension String {
  // TODO: note that a copy may happen if `String` is not native...
  public var utf8Span: UTF8Span {
    // TODO: how to do this well, considering we also have small 
    //       strings
  }
}
extension Substring {
  // TODO: needs scalar alignment (check Substring's invariants)
  // TODO: note that a copy may happen if `String` is not native...
  public var utf8Span: UTF8Span {
    // TODO: how to do this well, considering we also have small 
    //       strings
  }
}
```

Additionally, we extend `RawSpan`'s byte parsing support with helpers for parsing validly-encoded UTF-8.

```swift
extension RawSpan {
  public func parseUTF8(
    _ position: inout Index, length: Int
  ) throws -> UTF8Span

  public func parseNullTermiantedUTF8(
    _ position: inout Index
  ) throws -> UTF8Span
}

extension RawSpan.Cursor {
  public mutating func parseUTF8(length: Int) throws -> UTF8Span

  public mutating func parseNullTermiantedUTF8() throws -> UTF8Span
}
```

## Source compatibility

This proposal is additive and source-compatible with existing code.

## ABI compatibility

This proposal is additive and ABI-compatible with existing code.

## Implications on adoption

The additions described in this proposal require a new version of the standard library and runtime.

## Future directions


### More alignments

Future API could include whether an index is "word aligned" (either [simple](https://www.unicode.org/reports/tr18/#Simple_Word_Boundaries) or [default](https://www.unicode.org/reports/tr18/#Default_Word_Boundaries)), "line aligned", etc.

### Normalization

Future API could include checks for whether the content is in a normal form. These could take the form of thorough checks, quick checks, and even mutating check-and-update-flag checks.

### Transcoded views, normalized views, case-folded views, etc

We could provide lazily transcoded, normalized, case-folded, etc., views. If we do any of these for `UTF8Span`, we should consider adding equivalents on `String`, `Substring`, etc.

For example, transcoded views can be generalized:

```swift
extension UTF8Span {
  /// A view off the span's contents as a bidirectional collection of 
  /// transcoded `Encoding.CodeUnit`s.
  @frozen
  public struct TranscodedView<Encoding: _UnicodeEncoding> {
    public var span: UTF8Span

    @inlinable
    public init(_ span: UTF8Span)

    ...
  }
}
```

Note: UTF-16 has such historical significance that, even with a fully-generic transcoded view, we'd still want a dedicated, specialized type for UTF-16.

We could similarly provide lazily-normalized views of code units or scalars under NFC or NFD (which the stdlib already distributes data tables for), possibly generic via a protocol for 3rd party normal forms.

Finally, case-folded functionality can be accessed in today's Swift via [scalar properties](https://developer.apple.com/documentation/swift/unicode/scalar/properties-swift.struct), but we could provide convenience collections ourselves as well.


### Regex or regex-like support

Future API additions would be to support `Regex`es on such spans. 

Another future direction could be to add many routines corresponding to the underlying operations performed by the regex engine, such as:

```swift
extension UTF8Span.CharacterView {
  func matchCharacterClass(
    _: CharacterClass,
    startingAt: Index,
    limitedBy: Index    
  ) throws -> Index?

  func matchQuantifiedCharacterClass(
    _: CharacterClass,
    _: QuantificationDescription,
    startingAt: Index,
    limitedBy: Index    
  ) throws -> Index?
}
```

which would be useful for parser-combinator libraries who wish to expose `String`'s model of Unicode by using the stdlib's accelerated implementation.


### Index rounding operations

Unlike String, `UTF8Span`'s view's `Index` types are distinct, which avoids a [mess of problems](https://forums.swift.org/t/string-index-unification-vs-bidirectionalcollection-requirements/55946). Interesting additions to both `UTF8Span` and `String` would be explicit index-rounding for a desired behavior.

### Canonical Spaceships

Should a `ComparisonResult` (or [spaceship](https://forums.swift.org/t/pitch-comparison-reform/5662)) be added to Swift, we could support that operation under canonical equivalence in a single pass rather than subsequent calls to `isCanonicallyEquivalent(to:)` and `isCanonicallyLessThan(_:)`.


### Other Unicode functionality

For the purposes of this pitch, we're not looking to expand the scope of functionality beyond what the stdlib already does in support of `String`'s API. Other functionality can be considered future work.


## Alternatives considered



### Use the same Index type across views




### Deprecate `String.withUTF8`

... mutating... 

### Alternate places or representations for UTF-8 `EncodingError`s

**TODO**: Should `EncodingError.range` be a range of span indices instead, and we only have a span-based init? Should it be generic over the index type? Should it be inside of `Unicode.UTF8` instead?



- put it on `UTF8.EncodingError`
- make it generic over index type 
  - (but doesn't necessarily make more sense for null-terminated UTF-8 pointer)




### An unsafe UTF8 Buffer Pointer type

An [earlier pitch](https://forums.swift.org/t/pitch-utf-8-processing-over-unsafe-contiguous-bytes/69715) proposed an unsafe version of `UTF8Span`. 

...

## Acknowledgments

Karoy Lorentey, Karl, Geordie_J, and fclout, contributed to this proposal with their clarifying questions and discussions.

