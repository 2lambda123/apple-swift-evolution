# Generic Subscripts

* Proposal: [SE-NNNN](NNNN-filename.md)
* Authors: [Chris Eidhof](http://github.com/chriseidhof/)
* Review Manager: TBD
* Status: **Awaiting review**

* Bugs: [SR-115](https://bugs.swift.org/browse/SR-115?jql=text%20~%20%22Generic%20subscript%22)

## Introduction

Make it possible to have generic subscripts. Example:

```swift
extension Collection {
  subscript<Indices: Sequence>(indices: Indices) -> [Iterator.Element] where Indices.Iterator.Element == Index {
    // ...
  }
}
```

Or a generic return type:

```swift
extension JSON {
  subscript<T: JSONConvertible>(key: String) -> T? {
    // ...
  }
}
```

Swift-evolution thread: [Generic Subscripts](https://lists.swift.org/pipermail/swift-evolution/Week-of-Mon-20170109/030064.html).

## Motivation

Currently, subscripts can't be generic. This is limiting in a number of ways: 

- Some subscripts are very specific and could be made more generic.
- Some generic methods would feel more natural as a subscript, but currently can't be. This also makes it impossible to use them as lvalues.

This feature is also mentioned in the generics manifesto under [generic subscripts](https://github.com/apple/swift/blob/master/docs/GenericsManifesto.md#generic-subscripts).

## Proposed solution

Add generics to subscripts. There are two pieces to this: where to add the generic parameter list, and where to add the `where`-clause. The most straightforward way would be to use the same syntax as methods:

```swift
extension Dictionary {
  subscript<Indices: Sequence>(indices: Indices) -> [Iterator.Element] where Indices.Iterator.Element == Index {
    // ...
  }
}
```

## Source compatibility

This is a purely additive change. We don't propose changing the Standard Library to use this new feature, that should be part of a separate proposal. (Likewise, we should consider making subscripts `throws` in a [separate proposal](https://github.com/brentdax/swift-evolution/blob/throwing-properties/proposals/0000-throwing-properties.md)).

## Effect on ABI stability

It won’t change the ABI of existing subscript calls.

## Effect on API resilience

It won’t change the ABI of existing subscript calls, but if the standard library introduces new generic subscripts that replace older non-generic subscripts, it will impact ABI.

## Alternatives considered

None.
