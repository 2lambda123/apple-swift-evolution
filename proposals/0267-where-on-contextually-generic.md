# `where` clauses on contextually generic declarations

* Proposal: [SE-0267](0267-where-on-contextually-generic.md)
* Author: [Anthony Latsis](https://github.com/AnthonyLatsis)
* Review Manager: [Joe Groff](https://github.com/jckarter)
* Status: **Active review (October 21...October 31)**
* Implementation: [apple/swift#23489](https://github.com/apple/swift/pull/23489)

## Introduction

This proposal aims to lift the mostly artificial restriction on attaching `where` clauses to declarations that reference only outer generic parameters. Simply put, this means you no longer have to worry about the `'where' clause cannot be attached` error inside most generic contexts.

```swift
struct Box<Wrapped> {
    func sequence() -> [Box<Wrapped.Element>] where Wrapped: Sequence { ... }
}

```

> Only declarations that already support a generic parameter list will be allowed to carry a `where` clause inside a generic 
> context. Generic properties and constraints on protocol requirements are out of scope for the current proposal.
> For instance, the following remains an error:
> ```swift
> protocol P {
>     // error: Instance method requirement 'foo(arg:)' cannot add constraint 'Self: Equatable' on 'Self'
>     func foo() where Self: Equatable  
> }
> ```
> Whereas placing a constraint on an extension member rather than the extension itself becomes possible:
> ```swift
> extension P {
>     func bar() where Self: Equatable { }
> }
> ```

Swift-evolution thread: [Discussion thread topic for that proposal](https://forums.swift.org/t/where-clauses-on-contextually-generic-declaractions/22449)

## Motivation

Today, `where` clauses on contextually generic declarations, including protocol extension members, are expressed indirectly by placing them inside conditional extensions. Unless constraints are identical, every such declaration requires a separate extension.
This dependence on extensions can be an obstacle to grouping semantically related APIs, stacking up constraints, and sometimes the legibility of heavily generic interfaces. 

It is reasonable to expect a `where` clause to work anywhere a constraint can be meaningfully imposed, that is, both of these structuring styles should be available to the user:

```swift
// 'Foo' can be any kind of nominal type declaration.
// For a protocol, 'T' would be Self or an associatedtype.
struct Foo<T>  

extension Foo where T: Sequence, T.Element: Equatable {
    func slowFoo() { ... }
}
extension Foo where T: Sequence, T.Element: Hashable {
    func optimizedFoo() { ... }
}
extension Foo where T: Sequence, T.Element == Character {
    func specialCaseFoo() { ... }
}

extension Foo where T: Sequence, T.Element: Equatable {
    func slowFoo() { ... }

    func optimizedFoo() where T.Element: Hashable { ... }

    func specialCaseFoo() where T.Element == Character { ... }
}
```
A step towards generalizing `where` clause usage is an obvious and farsighted improvement to the generics
system with numerous future applications, including generic properties, [opaque types](https://github.com/apple/swift-evolution/blob/master/proposals/0244-opaque-result-types.md), [generalized
existentials](https://github.com/apple/swift/blob/master/docs/GenericsManifesto.md#generalized-existentials) and constrained protocol requirements. 

## Source compatibility and ABI stability

This is an additive change with no impact on the ABI and existing code.

## Effect on API resilience

For public declarations in resilient libraries such as the Standard Library, moving a constraint from an extension to a member and vice versa will break the ABI due to subtle mangling differences as of the current implementation.
