# Conditional conformances

* Proposal: [SE-0143](0143-conditional-conformances.md)
* Author: [Doug Gregor](https://github.com/DougGregor)
* Review Manager: [Joe Groff](https://github.com/jckarter)
* Status: **Active review (September 28...October 7)**

## Introduction

Conditional conformances express the notion that a generic type will
conform to a particular protocol only when its type arguments meet
certain requirements. For example, the `Array` collection can
implement the `Equatable` protocol only when its elements are
themselves `Equatable`, which can be expressed via the following
conditional conformance on `Equatable`:

```swift
extension Array: Equatable where Element: Equatable {
  static func ==(lhs: Array<Element>, rhs: Array<Element>) -> Bool { ... }
}
```

This feature is part of the [generics
manifesto](https://github.com/apple/swift/blob/master/docs/GenericsManifesto.md#conditional-conformances-)
because it's something that fits naturally into the generics model and
is expected to have a high impact on the Swift standard library.

Swift-evolution thread: [here](https://lists.swift.org/pipermail/swift-evolution/Week-of-Mon-20160926/027300.html)

## Motivation

Conditional conformances address a hole in the composability of the
generics system. Continuing the `Array` example from above, it's
always been the case that one could use the `==` operator on two
arrays of `Equatable` type, e.g., `[Int] == [Int]` would
succeed. However, it doesn't compose: arrays of arrays of `Equatable`
types cannot be compared (e.g., `[[Int]] == [[Int]]` will fail to
compile) because, even though there is an `==` for arrays of
`Equatable` type, the arrays themselves are never `Equatable`.

Conditional conformances are particularly powerful when building
generic adapter types, which are intended to reflect the capabilities
of their type arguments. For example, consider the "lazy"
functionality of the Swift standard library's collections: using the
`lazy` member of a sequence produces a lazy adapter that conforms to
the `Sequence` protocol, while using the `lazy` member of a collection
produces a lazy adapter that conforms to the `Collection`
protocol. In Swift 3, the only way to model this is with different
types. For example, the Swift standard library has four similar
generic types to handle a lazy collection: `LazySequence`,
`LazyCollection`, `LazyBidirectionalCollection`, and
`LazyRandomAccessCollection`. The Swift standard library uses
overloading of the `lazy` property to decide among these:

```swift
extension Sequence {
  var lazy: LazySequence<Self> { ... }
}

extension Collection {
  var lazy: LazyCollection<Self> { ... }
}

extension BidirectionalCollection {
  var lazy: LazyBidirectionalCollection<Self> { ... }
}

extension RandomAccessCollection {
  var lazy: LazyRandomAccessCollection<Self> { ... }
}
```

This approach causes an enormous amount of repetition, and doesn't
scale well because each more-capable type has to re-implement (or
somehow forward the implementation of) all of the APIs of the
less-capable versions. With conditional conformances, one can provide
a single generic wrapper type whose basic requirements meet the lowest
common denominator (e.g., `Sequence`), but which scale their
capabilities with their type argument (e.g., the `LazySequence`
conforms to `Collection` when the type argument does, and so on).

## Proposed solution
In a nutshell, the proposed solution is to allow a constrained
extension of a `struct`, `enum`, or `class` (but [not a protocol](#alternatives-considered)) to declare protocol
conformances. No additional syntax is necessary for this change,
because it already exists in the grammar; rather, this proposal
removes the limitation that results in the following error:

```
t.swift:1:1: error: extension of type 'Array' with constraints cannot have an inheritance clause
extension Array: Equatable where Element: Equatable { }
^                ~~~~~~~~~
```

Conditional conformances can only be used when the additional
requirements of the constrained extension are satisfied. For example,
given the aforementioned `Array` conformance to `Equatable`:

```swift
func f<T: Equatable>(_: T) { ... }

struct NotEquatable { }

func test(a1: [Int], a2: [NotEquatable]) {
  f(a1)    // okay: [Int] conforms to Equatable because Int conforms to Equatable
  f(a2)    // error: [NotEquatable] does not conform to Equatable because NotEquatable has no conformance to Equatable
}
```

Conditional conformances also have a run-time aspect, because a
dynamic check for a protocol conformance might rely on the evaluation
of the extra requirements needed to successfully use a conditional
conformance. For example:

```swift
protocol P {
  func doSomething()
}

struct S: P {
  func doSomething() { print("S") }
}

// Array conforms to P if it's element type conforms to P
extension Array: P where Element: P {
  func doSomething() {
    for value in self {
      value.doSomething()
    }
  }
}

// Dynamically query and use conformance to P.
func doSomethingIfP(_ value: Any) {
  if let p = value as? P {
    p.doSomething()
  } else {
    print("Not a P")
  }
}

doSomethingIfP([S(), S(), S()]) // prints "S" three times
doSomethingIfP([1, 2, 3])       // prints "Not a P"
```

The `if-let` in `doSomethingIfP(_:)` dynamically queries whether the type stored in `value` conforms to the protocol `P`. In the case of an `Array`, that conformance is conditional, which requires another dynamic lookup to determine whether the element type conforms to `P`: in the first call to `doSomethingIfP(_:)`, the lookup finds the conformance of `S` to `P`. In the second case, there is no conformance of `Int` to `P`, so the conditional conformance cannot be used. The desire for this dynamic behavior motivates some of the design decisions in this proposal.

## Detailed design
Most of the semantics of conditional conformances are
obvious. However, there are a number of issues (mostly involving
multiple conformances) that require more in-depth design.

### Disallow overlapping conformances
With conditional conformances, it is possible to express that a given
generic type can conform to the same protocol in two different ways,
depending on the capabilities of its type arguments. For example:

```swift
struct SomeWrapper<Wrapped> {
  let wrapped: Wrapped
}

protocol HasIdentity {
  static func ===(lhs: Self, rhs: Self) -> Bool
}

extension SomeWrapper: Equatable where Wrapped: Equatable {
  static func ==(lhs: SomeWrapper<Wrapped>, rhs: SomeWrapper<Wrapper>) -> Bool {
    return lhs.wrapped == rhs.wrapped
  }
}

extension SomeWrapper: Equatable where Wrapped: HasIdentity {
  static func ==(lhs: SomeWrapper<Wrapped>, rhs: SomeWrapper<Wrapper>) -> Bool {
    return lhs.wrapped === rhs.wrapped
  }
}
```

Note that, for an arbitrary type `T`, there are four potential answers to
the question of whether `SomeWrapper<T>` conforms to `Equatable`:

1. No, it does not conform because `T` is neither `Equatable` nor
`HasIdentity`.
2. Yes, it conforms via the first extension of `SomeWrapper` because
`T` conforms to `Equatable`.
3. Yes, it conforms via the second extension of `SomeWrapper` because
`T` conforms to `HasIdentity`.
4. Ambiguity, because `T` conforms to both `Equatable` and
`HasIdentity`.

It is due to the possibility of #4 occurring that we refer to the two conditional conformances in the example as *overlapping*. There are designs that would allow one to address the ambiguity, for example, by writing a third conditional conformance that addresses #4:

```swift
// Possible tie-breaker conformance
extension SomeWrapper: Equatable where Wrapped: Equatable & HasIdentity, {
  static func ==(lhs: SomeWrapper<Wrapped>, rhs: SomeWrapper<Wrapper>) -> Bool {
    return lhs.wrapped == rhs.wrapped
  }
}
```

The design is consistent, because this third conditional conformance is more *specialized* than either of the first two conditional conformances, meaning that its requirements are a strict superset of the requirements of those two conditional conformances. However, there are a few downsides to such a system:

1. To address all possible ambiguities, one has to write a conditional conformance for every plausible combination of overlapping requirements. To *statically* resolve all ambiguities, one must also cover nonsensical combinations where the two requirements are mutually exclusive (or invent a way to state mutual-exclusivity).
2. It is no longer possible to uniquely say what is required to make a generic type conform to a protocol, because there might be several unrelated possibilities. This makes reasoning about the whole system more complex, because it admits divergent interfaces for the same generic type based on their type arguments. At its extreme, this invites the kind of cleverness we've seen in the C++ community with template metaprogramming, which is something Swift has sought to avoid.
3. All of the disambiguation machinery required at compile time (e.g., to determine whether one conditional conformance is more specialized than another to order them) also needs to implements in the run-time, as part of the dynamic casting machinery. One must also address the possibility of ambiguities occurring at run-time. This is both a sharp increase in the complexity of the system and a potential run-time performance hazard.

For these reasons, this proposal *bans overlapping conformances* entirely. While the resulting system is less flexible than one that allowed overlapping conformances, the gain in simplicity in this potentially-confusing area is well worth the cost. Moreover, this ban follows with existing Swift rules regarding multiple conformances, which prohibit the same type from conforming to the same protocol in two different ways:

```swift
protocol P { }

struct S : P { }
extension S : P { } // error: S already conforms to P
```

### Implied conditional conformances 

Stating conformance to a protocol implicitly states conformances to any of the protocols that it inherits. This is the case in Swift today, although most developers likely don't realize the rules it follows. For example:

```swift
protocol P { }
protocol Q : P { }
protocol R : P { }

struct X1 { }
struct X2 { }
struct X3 { }

extension X1: Q { }  // implies conformance to P

extension X2: Q { }  // would imply conformance to P, but...
extension X2: P { }  // explicitly-stated conformance to P "wins"

extension X3: Q { }  // implies conformance to P
extension X3: R { }  // also implies conformance to P
                     // one will "win"; which is unspecified
```

With conditional conformances, the question of which extension "wins" the implied conformance begins to matter, because the extensions might have different constraints on them. For example:

```swift
struct X4<T> { }

extension X4: Q where T: Q { }  // implies conformance to P
extension X4: R where T: R { }  // error: implies overlapping conformance to P
```

Both of these constrained extensions imply a conformance to `P`, but the actual `P` implied conformances to `P` are overlapping and, therefore, result in an error.

However, in cases where there is a reasonable ordering between the two constrained extensions (i.e., one is more specialized than the other), the less specialized constrained extension should "win" the implied conformance. Continuing the example from above:

```swift
protocol S: R { }

struct X5<T> { }

extension X5: S where T: S { }

// This last extension "wins" the implied conformance to P, because
// the extension where "T: R" is less specialized than the one
// where "T: S".
extension X5: R where T: R { }
```

Thus, the rule for placing implied conformances is to pick the *least specialized* extension that implies the conformance. If there is more than one such extension, then either:

1. All such extensions are not constrained extensions (i.e., they have no requirements beyond what the type requires), in which case Swift can continue to choose arbitrarily among the extensions, or
2. All such extensions are constrained extensions, in which case the program is ill-formed due to the ambiguity. The developer can explicitly specify conformance to the protocol to disambiguate. 

### Overloading across constrained extensions

One particularly important aspect of the placement rule for implied conformances is that it affects which declarations are used to satisfy a particular requirement. For example:


```swift
protocol P {
  func f()
}

protocol Q: P { }
protocol R: Q { }

struct X1<T> { }

extension X1: Q where T: Q {           // note: implied conformance to P here
  func f() {
    // #1: basic implementation of 'f()'
  }
}

extension X1: R where T: R {
  func f() {
    // #2: superfast implementation of f() using some knowledge of 'R'
  }
}

struct X2: R {
  func f() { }
}

(X1<X2>() as P).f() // calls #1, which was used to satisfy the requirement for 'f'
X1<X2>().f()        // calls #2, which is preferred by overload resolution
```

Effectively, when satisfying a protocol requirement, one can only choose from members of the type that are guaranteed to be available within the extension with which the conformance is associated. In this case, the conformance to `P` is placed on the first extension of `X1`, so the only `f()` that can be considered is the `f()` within that extension: the `f()` in the second extension won't necessarily always be available, because `T` may not conform to `R`. Hence, the call that treats an `X1<X2>` as a `P` gets the first implementation of `X1.f()`. When using the concrete type `X1<X2>`, where `X2` conforms to `R`, both `X1.f()` implementations are visible... and the second is more specialized.

Technically, this issue is no different from surprises where (e.g.) a member added to a concrete type in a different module won't affect an existing protocol conformance. The existing ideas to mediate these problems---warning for nearly-matching functions when they are declared in concrete types, for example---will likely be sufficient to help surprised users. That said, this proposal may increase the likelihood of such problems showing up.

## Source compatibility

From the language perspective, conditional conformances are purely additive. They introduce no new syntax, but instead provide semantics for existing syntax---an extension that both declares a protocol conformance and has a `where` clause---whose use currently results in a type checker failure. That said, this is a feature that is expected to be widely adopted within the Swift standard library, which may indirectly affect source compatibility.

## Effect on ABI Stability

As noted above, there are a number of places where the standard library is expected to adopt this feature, which fall into two classes:

1. Improve composability: the example in the [introduction](#introduction) made `Array` conform to `Equatable` when its element type does; there are many places in the Swift standard library that could benefit from this form of conditional conformance, particularly so that collections and other types that contain values (e.g., `Optional`) can compose better with generic algorithms. Most of these changes won't be ABI- or source-breaking, because they're additive.
2. Eliminating repetition: the `lazy` wrappers described in the [motivation](#motivation) section could be collapsed into a single wrapper with several conditional conformances. A similar refactoring could also be applied to the range abstractions and slice types in the standard library, making the library itself simpler and smaller. All of these changes are potentially source-breaking and ABI-breaking, because they would remove types that could be used in Swift 3 code. However, there are mitigations: generic typealiases could provide source compatibility to Swift 3 clients, and the ABI-breaking aspect is only relevant if conditional conformances and the standard library changes they imply aren't part of Swift 4.

Aside from the standard library, conditional conformances have an impact on the Swift runtime, which will require specific support to handle dynamic casting. If that runtime support is not available once ABI stability has been declared, then introducing conditional conformances in a later language version either means the feature cannot be deployed backward or that it would provide only more limited, static behavior when used on older runtimes. Hence, there is significant motivation for doing this feature as part of Swift 4. Even if we waited to introduce conditional conformances, we would want to include a hook in the runtime to allow them to be implemented later, to avoid future backward-compatibility issues.

## Effect on Resilience

One of the primary goals of Swift 4 is resilience, which allows libraries to evolve without breaking binary compatibility with the applications that use them. While the specific details of the impact of conditional conformances on resilience will be captured in a more-complete proposal on resilience, possible rules are summarized here:

* A conditional conformance cannot be removed in the new version of a library, because existing clients might depend on it.
* A conditional conformance can be added in a new version of a library, roughly following the rules described in the [library evolution document](https://github.com/apple/swift/blob/master/docs/LibraryEvolution.rst#new-conformances). The conformance itself will need to be annotated with the version in which it was introduced.
* A conditional conformance can be *generalized* in a new version of the library, i.e., it can be effectively replaced by a (possibly conditional) conformance in a new version of the library that is less specialized than the conditional conformance in the older version of the library. For example.

  ```swift
  public struct X<T> { }
  
  // Conformance in version 1.0
  public extension X: Sequence where T: Collection { ... }
  
  // Can be replaced by this less-specialized conformance in version 1.1
  public extension X: Sequence where T: Sequence { ... }
  ```
  
  Such conformances would likely need some kind of annotation.

## Alternatives considered

The most common request related to conditional conformances is to allow a (constrained) protocol extension to declare conformance to a protocol. For example:

```swift
extension Collection: Equatable where Iterator.Element: Equatable {
  static func ==(lhs: Self, rhs: Self) -> Bool {
    // ...
  }
}
```

This protocol extension will make any `Collection` of `Equatable` elements `Equatable`, which is a powerful feature that could be put to good use. Introducing conditional conformances for protocol extensions would exacerbate the problem of overlapping conformances, because it would be unreasonable to say that the existence of the above protocol extension means that no type that conforms to `Collection` could declare its own conformance to `Equatable`, conditional or otherwise.

There are several potential solutions to the problem of overlapping conformances (e.g., admitting some form of overlapping conformances that can be resolved at runtime or introducing the notion of conformances that cannot be queried a runtime), but the feature is large enough to warrant a separate proposal that explores the solutions in greater depth.
