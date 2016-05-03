# More Powerful Constraints for Associated Types

* Proposal: [SE-XXXX](https://github.com/apple/swift-evolution/blob/master/proposals/XXXX-associated-types-constraints.md)
* Author(s): [David Hart](http://github.com/hartbit), [Jacob Bandes-Storch](jtbandes@gmail.com), [Douglas Gregor](dgregor@apple.com)
* Status: TBD
* Review manager: TBD

## Introduction

This proposal seeks to introduce a `where` clause to associated types declarations and improvements to protocol constraints to bring associated types the same expressive power as generic types.

This proposal was discussed twice on the Swift Evolution list in the following threads:

- [[Completing Generics] Arbitrary requirements in protocols](http://thread.gmane.org/gmane.comp.lang.swift.evolution/14243)
- [More Powerful Constraints for Associated Types](http://thread.gmane.org/gmane.comp.lang.swift.evolution/15201)
 
## Motivation

Currently, associated type declarations can only express simple inheritance constraints and not the more sophisticated constraints available to generic types with the `where` clause. Some designs, including many in the Standard Library, require more powerful constraints for associated types to be truly elegant. For example, the `SequenceType` protocol can be declared as follows:

```swift
protocol Sequence {
    associatedtype Iterator : IteratorProtocol
    associatedtype SubSequence : Sequence where SubSequence.Iterator.Element == Iterator.Element
    ...
}
```

## Detailed Design

First of all, this proposal modifies the grammar for protocols associated types to the following:

*protocol-associated-type-declaration* → *attributes<sub>opt</sub>* *access-level-modifier<sub>opt</sub>* **associatedtype** *typealias-name* ­*type-inheritance-clause­<sub>opt</sub>* *typealias-assignment­<sub>opt</sub>* *requirement-clause<sub>opt</sub>*

The new requirement-clause is then used by the compiler to validate the associated types of conforming types.

Secondly, the proposal also allows protocols to use the associated types of their conforming protocols in their declaration where clause as below:

```swift
protocol IntSequence : Sequence where Iterator.Element == Int {
    ...
}
```

Name lookup semantics in the protocol declaration `where` clause only looks at associated types in the parent protocols. For example, the following code would cause an error:

```swift
protocol IntSequence : Sequence where Counter: Int { // error: Use of undefined associated type 'Counter'
    associatedtype Counter
}
```

But instead should be written on the associated type itself:

```swift
protocol IntSequence : Sequence {
    associatedtype Counter: Int
}
```
 
## Alternatives

Douglas Gregor argues that the proposed syntax is redundant when adding new constraints to an associated type declared in a parent protocol and proposes another syntax: 

```swift
protocol Collection : Sequence {
    where SubSequence : Collection
}
```

But as Douglas notes himself, that syntax will become ambiguous if we adopt the generic where clause at the end of declarations like discussed in proposal [SE-0081: Move where clause to end of declaration](https://github.com/apple/swift-evolution/blob/master/proposals/0081-move-where-expression.md). For those reasons, it might be wiser not to introduce the shorthand syntax.
 
## Acknowledgements

Thanks to Dave Abrahams and Douglas Gregor for taking the time to help me through this proposal.