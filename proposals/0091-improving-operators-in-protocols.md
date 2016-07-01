# Improving operator requirements in protocols

* Proposal: [SE-0091](0091-improving-operators-in-protocols.md)
* Author: [Tony Allevato](https://github.com/allevato), [Doug Gregor](https://github.com/DougGregor)
* Status: **Active Review: May 17...23**
* Review manager: [Chris Lattner](http://github.com/lattner)

## Introduction

When a type conforms to a protocol that declares an operator as a requirement,
that operator must be implemented as a global function defined outside of the
conforming type. This can lead both to user confusion and to poor type checker
performance since the global namespace is overcrowded with a large number of
operator overloads. This proposal mitigates both of those issues by proposing
that operators in protocols be declared statically (to change and clarify where
the conforming type implements it) and that Swift use universal lookup for
operators that finds candidates both at the global scope and within types.

Swift-evolution thread:
[Discussion about operators and protocols in the context of `FloatingPoint`](https://lists.swift.org/pipermail/swift-evolution/Week-of-Mon-20160425/015807.html)

## Motivation

The proposal came about as a result of discussion about
[SE-0067: Enhanced Floating Point Protocols](https://github.com/apple/swift-evolution/blob/master/proposals/0067-floating-point-protocols.md).
To implement the numerous arithmetic and comparison operators, this protocol
defined named instance methods for them and then implemented the global operator
functions to delegate to them. For example,

```swift
public protocol FloatingPoint {
  func adding(rhs: Self) -> Self
  // and others
}

public func + <T: FloatingPoint>(lhs: T, rhs: T) -> T {
  return lhs.adding(rhs)
}
```

One of the motivating factors for these named methods was to make the operators
generic and reduce the number of concrete global overloads, which would improve
the type checker's performance compared to individual concrete overloads for
each conforming type. Some concerns were raised about the use of named methods:

* They bloat the public interface. Every floating point type would expose
  mutating and non-mutating methods for each arithmetic operation, as well as
  non-mutating methods for the comparisons. We don't expect users to actually
  call these methods directly but they must be present in the public interface
  because they are requirements of the protocol. Therefore, they would clutter
  API documentation and auto-complete lists and make the properties and methods
  users actually want to use less discoverable.
* Swift's naming guidelines encourage the use of "terms of art" for naming when
  it is appropriate. In this case, the operator itself is the term of art. It
  feels odd to elevate `(2.0).adding(2.0).isEqual(to: 4.0)` to the same
  first-class status as `2.0 + 2.0 == 4.0`; this is the situation that
  overloaded operators were made to prevent.
* Devising good names for the operators is tricky; the swift-evolution list had
  a fair amount of bikeshedding about the naming and preposition placement of
  `isLessThanOrEqual(to:)` in order to satisfy API guidelines, for example.
* Having both an `adding` method and a `+` operator provides two ways for the
  user to do the same thing. This may lead to confusion if users think that
  the two ways of adding have slightly different semantics.

Some contributors to the discussion list have expressed concerns about operators
being members of protocols at all. I feel that removing them entirely would be a
step backwards for the Swift language; a protocol is not simply a list of
properties and methods that a type must implement, but rather a higher-level set
of requirements. Just as properties, methods, and associated types are part of
that requirement set, it makes sense that an arithmetic type, for example, would
declare arithmetic operators among its requirements as well.

### Inconsistency in the current operator design with protocols

When a protocol declares an operator as a requirement, that requirement is
located _inside_ the protocol definition. For example, consider `Equatable`:

```swift
protocol Equatable {
  func ==(lhs: Self, rhs: Self) -> Bool
}
```

However, since operators are global functions, the actual implementation of that
operator for a conforming type must be made _outside_ the type definition. This
can look particularly odd when extending an existing type to conform to an
operator-only protocol:

```swift
extension Foo: Equatable {}

func ==(lhs: Foo, rhs: Foo) -> Bool {
  // Implementation goes here
}
```

This is an odd inconsistency in the Swift language, driven by the fact that
operators must be global functions. What's worse is that every concrete type
that conforms to `Equatable` must provide the operator function at global scope.
As the number of types conforming to this protocol increases, so does the
workload of the compiler to perform type checking.

## Proposed solution

When a protocol wishes to declare operators that conforming types must
implement, we propose adding the ability to declare operator requirements as
static members of the protocol:

```swift
protocol Equatable {
  static func ==(lhs: Self, rhs: Self) -> Bool
}
```

Types conforming to a protocol that contains static operators would implement
the operators as static methods (or class methods for class types) defined
_within_ the type:

```swift
struct Foo: Equatable {
  let value: Int

  static func ==(lhs: Foo, rhs: Foo) -> Bool {
    return lhs.value == rhs.value
  }
}

let f1 = Foo(value: 5)
let f2 = Foo(value: 10)
let eq = (f1 == f2)
```

We initially considered requiring users to declare a global "trampoline"
operator for each operator inside their protocols. This operator would be
generic and constrained to that protocol type and would use the static types of
its actual arguments to dispatch to the correct implementation. However, this
is a burden on protocol authors to provide these stub functions that are purely
an implementation detail.

Instead, Swift should always perform operator lookup universally such that
it sees all operators defined at either module scope or within a type/extension
of a type. This gives us the syntactic improvements immediately and the natural
Swift thing of defining your functionality within the type or an extension
thereof just works.

While it may seem odd that operators will be the only place where Swift does
such universal lookup, operators can be considered a special case. This is a
cleaner approach than requiring the user to manually provide trampoline
operators. There is really no way to avoid it: we simply don’t want normal
lexical name lookup for operators when they can be defined in types.

This approach does not (directly) give any of the type checker performance/QoI
improvements mentioned above. The key insight here is that we don't want to
consider both a generic operator based on some protocol (for example, `+` for
`Arithmetic` types) and the operator functions used to satisfy that
requirement.

Therefore, we can achieve the performance improvements by making
that insight part of the semantic model: when we find all operators, we also
find the operators in the protocols themselves. The operators in the protocols
are naturally generic; e.g., the `Arithmetic` `+` effectively has a generic
function type like this:

```
<Self: Arithmetic>(Self, Self) -> Self
```

Then, we say that we do not consider an operator function if it implements a
protocol requirement, because the requirement is a generalization of all of the
operator functions that satisfy that requirement. With this rule, we’re
effectively getting the same effects as if users had declared trampoline
operators, but it's automatic.

### Benefits of this approach

By using the name of the operator itself as the method, this approach avoids
bloating the public interfaces of protocols and conforming types with additional
named methods, reducing user confusion. This also will lead to better
consistency going forward, as various authors of such protocols will not be
providing their own method names.

This approach also significantly reduces the number of symbols in the global
namespace. Consider a protocol like `Equatable`, which requires a global
definition of `==` for _every_ type that conforms to it. The approach described
above with universal lookup will ignore all of the implementations of `==` on
types where it satisfies the `Equatable` conformance, which leaves only the
single operator on `Equatable` itself to be considered instead. (This assumes
that nobody implements `==` while not conforming to `Equatable`; while this is
certainly possible, it is likely to be rare enough that it would not negatively
impact performance.)

### Other kinds of operators (prefix, postfix, assignment)

Static operator methods have the same signatures as their global counterparts.
So, for example, prefix and postfix operators as well as assignment operators
would be defined the way one would expect:

```swift
protocol SomeProtocol {
  static func +=(lhs: inout Self, rhs: Self)
  static prefix func ~(value: Self) -> Self

  // These are deprecated, of course, but used here just to serve as an
  // example.
  static prefix func ++(value: inout Self) -> Self
  static postfix func ++(value: inout Self) -> Self
}
```

### Class types and inheritance

While this approach works well for value types, operators may not work as
expected for class types when inheritance is involved. We expect classes to
implement the static operators in the protocol using `class` methods instead of
`static` methods, which allows subclases to override them. However, note that
this requires the subclass's method signature to match the superclass's,
meaning that `Base.==(lhs: Base, rhs: Base)` would have to be overridden using
`Subclass.==(lhs: Base, rhs: Base)` (note the parameter types).

Note, however, that operators as implemented today have similar issues. For
example, the lack of multiple dispatch means that a comparison between a
`Subclass` and a `Subclass as Base` would call `==(Base, Base)`, even if there
exists a more specific `==(Subclass, Subclass)`. We acknowledge that this is a
problem in both cases and do not address it in this proposal, since the proposed
model is not a regression of current behavior.

For users who wish to go down this path, we should ensure that expressions using
`super`, like `super.==(lhs, rhs)`, work as expected inside such class methods.

### Deprecation of non-static protocol operators

Because the proposed solution serves as a replacement and improvement for the
existing syntax used to declare operator requirements in protocols, we propose
that the non-static operator method syntax be **deprecated** in Swift 2 and
**removed** in Swift 3. In Swift 3, static member operators should be the _only_
way to define operators that are required for protocol conformance. This is a
breaking change for existing code, but supporting two kinds of operators with
different declaration and use syntax would lead to significant user confusion.

Global operator functions would be unaffected by this change. Users would still
be able to define them as before.

## Detailed design

Currently, the Swift language allows the use of operators as the names of
global functions and of functions in protocols. This proposal is essentially
asking to extend that list to include static/class methods of protocols and
concrete types.

Interestingly, the production rules themselves of the Swift grammar for function
declarations _already_ appear to support declaring static functions inside a
protocol or other type with names that are operators. In fact, declaring a
static operator function in a protocol works today (that is, the static modifier
is ignored).

However, defining such a function in a concrete type fails with the error
`operators are only allowed at global scope`.
[This area](https://github.com/apple/swift/blob/797260939e1f9e453ab49a5cc6e0a7b40be61ec9/lib/Parse/ParseDecl.cpp#L4444)
of `Parser::parseDeclFunc` appears to be the likely place to make a change to
allow this.

### Explicitly calling `super` for class operators

This proposal originally extended the _explicit-member-expression_ production
rule to support having _operator_ as well as _identifier_ following a
_postfix-expression_; this was a requirement to let users explicitly call the
concrete operator implementation from their trampoline function, using a
syntax like `SomeType.+(lhs, rhs)`.

While universal lookup makes it no longer necessary to call operators in this
way in the general case, one scenario may still need to be considered: calling
the superclass implementation of an operator `class` method from a subclass.

### Restrictions on methods with operator names

Since methods with operator names are now found as part of a universal lookup,
we restrict a few characteristics of their declarations as follows:

* Methods with operator names must be `static` (or `class`, inside classes).
  Non-static methods with operator names are an error. (_Special case: in a
  protocol, non-static operator methods are marked deprecated until removed._)

* Methods with operator names must satisfy the same function signature
  requirements as global operator functions (infix operators take two arguments,
  prefix/postfix operators take one argument, and so forth).

## Impact on existing code

The ability to declare operators as static/class functions inside a type is a
new feature and would not affect existing code.

Changing the way operators are declared in protocols (static instead of
non-static) would be a breaking change. As described above, we propose
deprecating the current non-static protocol operator syntax and then removing it
entirely in Swift 3.

Applying this change to the protocols already in the Swift standard library
(such as `Equatable`) would be a breaking change, because it would change the
way by which subtypes conform to that protocol. It might be possible to
implement a quick fix that hoists a global operator function into the subtype's
definition, either by making it static and moving the code itself or by wrapping
it in an extension.

## Alternatives considered

One alternative would be to do nothing. This would leave us with the problems
cited above:

* Concrete types either provide their own global operator overloads, potentially
  exploding the global namespace and increasing the workload of the type
  checker...
* ..._or_ they define generic operators that delegate to named methods, but
  those named methods bloat the public interface of the type.
* Furthermore, there is no consistency required for these named methods among
  different types; each can define its own, and subtle differences in naming can
  lead to user confusion.

Another alternative would be that instead of using static methods, operators
could be defined as instance methods on a type. For example,

```swift
protocol SomeProtocol {
  func +(rhs: Self) -> Self
}

struct SomeType: SomeProtocol {
  func +(rhs: SomeType) -> SomeType { ... }
}

func + <T: SomeProtocol>(lhs: T, rhs: T) -> T {
  return lhs.+(rhs)
}
```

There is not much to be gained by doing this, however. It does not solve the
dynamic dispatch problem for classes described above, and it would require
writing operator method signatures that differ from those of the global
operators because the first argument instead becomes the implicit `self`. As a
matter of style, when it doesn't necessarily seem appropriate to elevate one
argument of an infix operator—especially one that is commutative—to the special
status of "receiver" while the other remains an argument.

Likewise, commutative operators with heterogeneous arguments are more awkward to
implement if operators are instance methods. Consider a contrived example of a
`CustomStringProtocol` type that supports concatenation with `Character` using
the `+` operator, commutatively. With static operators, both versions of the
operator are declared in `CustomStringProtocol`, as one would expect:

```swift
protocol CustomStringProtocol {
  static func +(lhs: Self, rhs: Character) -> Self
  static func +(lhs: Character, rhs: Self) -> Self
}
```

Likewise, the implementation of both operators would be contained entirely
within the conforming types. If these were instance methods, it's unclear how
the version that has the `Character` argument on the left-hand side would be
expressed in the protocol, or how it would be implemented if an instance of
`Character` were the receiver. Would it be an extension on the `Character` type?
This would split the implementation of an operation that logically belongs to
`CustomStringProtocol` across two different locations in the code, which is
something we're trying to avoid.

## Acknowledgments

Thanks to Chris Lattner and Dave Abrahams for contributing to the early
discussions, particularly regarding the need to improve type checker performance
by genericizing protocol-based operators. Thanks also to Doug Gregor who
provided some incredibly valuable insight near the end of the review process
that was significant enough that I consider him now a coäuthor of the proposal.
