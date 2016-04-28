# Add scan, prefix(while:), drop(while:), and iterate to the stdlib

* Proposal: [SE-0045](0045-scan-takewhile-dropwhile.md)
* Author(s): [Kevin Ballard](https://github.com/kballard)
* Status: **Review scheduled for April 28...May 3, 2016**
* Review manager: [Chris Lattner](http://github.com/lattner)

## Introduction

Add 3 new `Sequence` functions `scan(_:combine:)`, `prefix(while:)`, and
`drop(while:)`, with overrides as appropriate on `Collection`,
`LazySequenceProtocol`, and `LazyCollectionProtocol`, as well as a global
function `iterate(_:apply:)`.

Swift-evolution thread:
[Proposal: Add scan, takeWhile, dropWhile, and iterate to the stdlib](http://thread.gmane.org/gmane.comp.lang.swift.evolution/1515)

## Motivation

The Swift standard library provides many useful sequence manipulators like
`dropFirst(_:)`, `filter(_:)`, etc. but it's missing a few common methods that
are quite useful.

## Proposed solution

Add the following extension to `Sequence`:

```swift
extension Sequence {
  /// Returns an array containing the results of
  ///
  ///     p.reduce(initial, combine: combine)
  ///
  /// for each prefix `p` of `self` in order from shortest to longest, starting
  /// with the empty prefix and ending with `self`.
  ///
  /// For example:
  ///
  ///     (1..<6).scan(0, combine: +) // [0, 1, 3, 6, 10, 15]
  ///
  /// - Complexity: O(N)
  func scan<T>(initial: T, @noescape combine: (T, Self.Generator.Element) throws -> T) rethrows -> [T]
}
```

Modify the declaration of `Sequence` with two new members:

```swift
protocol Sequence {
  // ...
  /// Returns a subsequence by skipping elements while `predicate` returns
  /// `true` and returning the remainder.
  func drop(@noescape while predicate: (Self.Generator.Element) throws -> Bool) rethrows -> Self.SubSequence
  /// Returns a subsequence containing the initial elements until `predicate`
  /// returns `false` and skipping the remainder.
  func prefix(@noescape while predicate: (Self.Generator.Element) throws -> Bool) rethrows -> Self.SubSequence
}
```

Also provide default implementations on `Sequence` that return `AnySequence`,
and default implementations on `Collection` that return a slice.

`LazySequenceProtocol` and `LazyCollectionProtocol` will also be extended with
implementations of `scan(_:combine:)`, `drop(while:)`, and `prefix(while:)`
that return lazy sequence/collection types. Like the lazy `filter(_:)`,
`drop(while:)` will perform the filtering when `startIndex` is accessed.

Add a global function:

```swift
/// Returns an infinite sequence of lazy applications of `apply` to the
/// previous value. For example:
///
///     iterate(1, apply: { $0 * 2 }) // yields: 1, 2, 4, 8, 16, 32, 64, ...
func iterate<T>(initial: T, apply: T -> T) -> IterateSequence<T>
```

## Detailed design

In addition to the above declarations, provide default implementations based on
`AnySequence`, similarly to how functions like `dropFirst(_:)` and `prefix(_:)`
are handled:

```swift
extension SequenceType {
  func drop(@noescape while predicate: (Self.Generator.Element) throws -> Bool) rethrows -> AnySequence<Self.Generator.Element>
  func prefix(@noescape while predicate: (Self.Generator.Element) throws -> Bool) rethrows -> AnySequence<Self.Generator.Element>
}
```

These default implementations produce an `AnySequence` that wraps an `Array`
(as the functions must be implemented eagerly to match expected behavior).

Provide default implementations on `Collection` as well:

```swift
extension Collection {
  func drop(@noescape while predicate: (Self.Generator.Element) throws -> Bool) rethrows -> Self.SubSequence
  func prefix(@noescape while predicate: (Self.Generator.Element) throws -> Bool) rethrows -> Self.SubSequence
}
```

Extend `LazySequenceProtocol` with lazy versions of the functions:

```swift
extension LazySequenceType {
  func scan<T>(initial: T, combine: (T, Self.Generator.Element) -> T) -> LazyScanSequence<Self.Elements, T>
  func drop(while predicate: (Self.Generator.Element) -> Bool) -> LazyDropWhileSequence<Self.Elements>
  func prefix(while predicate: (Self.Generator.Element) -> Bool) -> LazyPrefixWhileSequence<Self.Elements>
}
```

The types `LazyScanSequence`, `LazyDropWhileSequence`, and
`LazyPrefixWhileSequence` all conform to `LazySequenceProtocol`.

Extend `LazyCollectionProtocol` with collection variants for the functions:

```swift
extension LazyCollectionType {
  func scan<T>(initial: T, combine: (T, Self.Generator.Element) -> T) -> LazyScanCollection<Self.Elements, T>
  func drop(while predicate: (Self.Generator.Element) -> Bool) -> LazyDropWhileCollection<Self.Elements>
  func prefix(while predicate: (Self.Generator.Element) -> Bool) -> LazyPrefixWhileCollection<Self.Elements>
}
```

The types `LazyScanCollection`, `LazyDropWhileCollection`, and
`LazyPrefixWhileCollection` conform to `LazyCollectionProtocol`.

The type `IterateSequence` from the function `iterate(_:apply:)` conforms to
`Sequence`.

## Impact on existing code

None, this feature is purely additive.

## Alternatives considered

#### Naming

The names here are likely to cause some bikeshedding. Here are some alternatives
I've heard proposed:

* `suffixFrom(firstElementSatisfying:)` instead of `drop(while:)` – Not only is
  it rather long, it's also focusing on taking a suffix while the actual
  expected usage of the function is focused around skipping elements at the
  start. There's also the potential confusion around whether it's the first
  element from the beginning or the first element from the end (since the term
  "suffix" implies working from the end backwards).
* `skip(while:)` instead of `drop(while:)` – I'm actually partial to this one,
  but we'd need to rename `dropFirst(_:)` as well. The benefit of this is it
  removes the potential confusion around whether the method is mutating.
* `take(while:)` instead of `prefix(while:)` – This was actually the original
  name proposed, and it matches precedent from other languages, but I eventually
  decided that consistency with `prefix(_:)` was desired. However, there is an
  argument to be made that `prefix(while:)` is using the term "prefix" like a
  verb instead of a noun, and the verb form means something different.
* `prefix(to:)` instead of `prefix(while:)` – The name here doesn't make it
  obvious that the argument is a predicate, and this also requires inverting the
  meaning of the predicate which I don't like. The focus of this function is on
  retaining the initial elements that have a desired characteristic, which
  suggests that the predicate should describe the characteristic the desired
  elements have, not the inverse.
* `prefix(having:)` instead of `prefix(while:)` – Reasonable. I chose
  `prefix(while:)` for consistency with `drop(while:)` but `prefix(having:)`
  makes more grammatical sense (since we're using the noun meaning of prefix
  rather than the verb meaning).
* `reducedPrefixes(_:combine:)` instead of `scan(_:combine:)` – Seems somewhat
  awkward.

I haven't heard any alternative suggestions for `iterate(_:apply:)`.

#### `iterate(_:apply:)`

This function is really a special case of calling `scan(_:combine:)` on an
infinite sequence containing the `combine` closure repeated forever, but that's
rather awkward and not at all obvious. As an example, the expression
`seq.scan(0, combine: { $0 * 2 })`, assuming a function `repeatedForever(_:)`,
would look like `repeatedForever({ $0 * 2 }).lazy.scan(0, combine: { $1($0) })`.
And even there, the function `repeatedForever(_:)` could easily be argued to
actually just be a special case of `CollectionOfOne(_:).cycle()`, assuming a
method `.cycle()` that repeats a sequence forever. So that turns into
`CollectionOfOne({ $0 * 2 }).cycle().lazy.scan(0, combine: { $1($0) })`. Very
awkward and not something you'd really expect anyone to write on their own. But
the pattern expressed by this function actually turns up with reasonable
frequency when doing FP-style processing of sequences, so I think it's
reasonable to have a function to express it.

#### `unfold(_:apply:)`

One FP function that I did not propose, but could be quite useful, is
`unfold(_:apply:)`. This is a the dual of `reduce(_:combine:)`. It takes an
initial value and a function, and uses it to build a list. The function
signature would look like

```swift
func unfold<T,State>(initial: State, apply: State -> (T, State)?) -> UnfoldSequence<T>
```

The sequence would return each successive initial value from the `apply` closure
until it returns `nil`. For example:

```swift
unfold(10, apply: { $0 == 0 ? nil : ($0,$0-1) })
// returns: 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
```

With this function, `iterate(initial, apply: f)` is actually a special case of
`unfold(initial, { ($0, f($0)) })`, except that this version of it performs the
closure one step in advance. A more faithful version would look like
`unfold({ initial }, { let x = $0(); return (x, { f(x) }) })`, which is rather
awkward and requires creating a bunch of intermediate closures (which suggests
that it wouldn't be as efficient as `iterate(_:apply:)` is).

I did not propose this function because I believe it to be more esoteric and
less useful than the ones I did propose, and more likely to be rejected by the
community. Just because an FP function exists doesn't mean the Swift stdlib has
to include it. But I've put it in this Alternatives section because it is
related to the functions I am proposing.
