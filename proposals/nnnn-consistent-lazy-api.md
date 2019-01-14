# Consistent `lazy` API

- Proposal: [SE-NNNN](nnnn-consistent-lazy-api.md)
- Author: [Dennis Vennink](https://github.com/dennisvennink)
- Review Manager: To Be Determined
- Status: **Work In Progress**
- Implementation: [apple/swift#21793](https://github.com/apple/swift/pull/21793)
- Bug: [SR-5754](https://bugs.swift.org/browse/SR-5754)

## Introduction

This proposal addresses the absence of lazy implementations of some `Sequence`-related operations.

### Discussion

A formal Swift Evolution forum thread will be posted as soon as possible.

## Motivation

Lazy evaluation defers the computation of an operation until (a part of) its result is needed. In Swift, this strategy is implemented in two ways. The first, [lazy stored properties](https://docs.swift.org/swift-book/LanguageGuide/Properties.html#ID257), enables the user to defer the evaluation of a property's initial value until the first time it's called. The second, `Sequence`'s [`lazy`](https://developer.apple.com/documentation/swift/sequence/1641562-lazy) instance property, enables the user to lazily evaluate subsequent operations by only evaluation those elements that are needed. The latter is the primary focus of this proposal.

`lazy` works by wrapping a `Sequence` in a `LazySequence`. Both `LazySequence` and some of its operations conform to `LazySequenceProtocol` enabling subsequent lazily implemented operations:

```swift
print(type(of: sequence(first: 0, next: { $0 + 1 }).lazy.map(String.init)))
// Prints "LazyMapSequence<UnfoldSequence<Int, (Optional<Int>, Bool)>, String>"
```

However, not all of `LazySequenceProtocol`'s operations `return` types that conform to `LazySequenceProtocol`.

Here, we expect a `LazyMapSequence`, but instead we get an `Array<String>`:

```swift
print(type(of: sequence(first: 0, next: { $0 + 1 }).lazy.prefix(3).map(String.init)))
// Prints "Array<String>"
```

Using any of these operations will lead to a loss of laziness and the immediately evaluation of (a part of) the expression. In certain situations it might lead to less performant code or the usage of more memory than its lazy counterpart.

## Proposed Solution

I propose conforming all existing lazily implemented `Sequence`s to `LazySequenceProtocol`. For those inherited members from `Sequence` that don't contain a lazy implementation on `LazySequenceProtocol`, overloads and types, conforming to `LazySequenceProtocol`, that are conventionally prefixed with `Lazy`.

## Detailed Design

### Conform Existing `Sequence`s To `LazySequenceProtocol`

The following `Sequence`s can't conform to `LazySequenceProtocol` because they don't contain an underlying `Sequence` and therefore can't conform to both `Sequence` and `LazySequenceProtocol`:

- `AnyBidirectionalCollection`
- `AnyCollection`
- `AnyIterator`
- `AnyRandomAccessCollection`
- `AnySequence`
- `Array`
- `ArraySlice`
- `ClosedRange`
- `CollectionOfOne`
- `ContiguousArray`
- `Dictionary`
- `EmptyCollection`
- `EmptyCollection.Iterator`
- `KeyValuePairs`
- `PartialRangeFrom`
- `Range`
- `Repeated`
- `Set`
- `StrideThrough`
- `StrideTo`
- `String`
- `String.UnicodeScalarView`
- `String.UTF16View`
- `String.UTF8View`
- `Substring`
- `Substring.UnicodeScalarView`
- `Substring.UTF16View`
- `Substring.UTF8View`
- `UnfoldSequence`
- `Unicode.Scalar.UTF16View`
- `UnsafeBufferPointer`
- `UnsafeMutableBufferPointer`
- `UnsafeMutableRawBufferPointer`
- `UnsafeRawBufferPointer`
- `UnsafeRawBufferPointer.Iterator`

The following types can't conform to `LazySequenceProtocol` because they aren't sufficiently lazy:

 - `DropFirstSequence`
 - `DropWhileSequence`

`DropFirstSequence` can't conform to `LazySequenceProtocol` because `makeIterator()` pre-emptively drops the first `k` elements due to performance reasons. For the latter type a lazy implementation already exists.

The following `Sequence`s already conform to `LazySequenceProtocol` and require no modifications:

- `LazyDropWhileSequence`
- `LazyFilterSequence`
- `LazyMapSequence`
- `LazyPrefixWhileSequence`
- `LazySequence`
- `ReversedCollection`
- `Slice`

The following `Sequence`s can conform to `LazySequenceProtocol`:

- `LazyFilterSequence.Iterator`
- `LazyPrefixWhileSequence.Iterator`
- `LazyMapSequence.Iterator`

The following `Sequence`s can conform to `LazySequenceProtocol` on the condition that `Base` conforms to `LazySequenceProtocol`:

- `EnumeratedSequence`
- `EnumeratedSequence.Iterator`
- `FlattenSequence`
- `FlattenSequence.Iterator`
- `IteratorSequence`
- `JoinedSequence`
- `PrefixSequence`
- `ReversedCollection.Iterator`

The conformance of `FlattenSequence` to `LazySequenceProtocol` means we can remove `LazySequence` from the `return` types of `joined()` and `flatMap(_:)` on `LazySequenceProtocol`. The old implementations will be deprecated. It also requires the explicit declaration of `FlattenCollection.SubSequence`.

The following `Sequence`s can conform to `LazySequenceProtocol` on the condition that `Elements` conforms to `LazySequenceProtocol`:

- `DefaultIndices`
- `IndexingIterator`

Finally, `Zip2Sequence` can conform to `LazySequenceProtocol` on the condition that `Sequence1` and `Sequence2` conform to `LazySequenceProtocol`.

### Add Missing Implementations And Types

The following inherited members from `Sequence` don't contain an overloaded lazy implementation on `LazySequenceProtocol`:

- `dropFirst(_:)`
- `dropLast(_:)`
- `prefix(while:)`
- `reversed()`
- `shuffled()`
- `shuffled(using:)`
- `sorted()`
- `sorted(by:)`
- `split(maxSplits:omittingEmptySubsequences:isSeparator:)`
- `split(separator:maxSplits:omittingEmptySubsequences:)`
- `suffix(_:)`

## Source Compatibility

...

## Effect on ABI Stability

This proposal doesn't change the ABI of existing language features.

## Effect on API Resilience

...

## Alternatives Considered

No alternative approaches have been considered.
