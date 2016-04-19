# Standardize function type argument syntax to require parentheses

* Proposal: [SE-0066](https://github.com/apple/swift-evolution/blob/master/proposals/0066-standardize-function-type-syntax.md)
* Author: [Chris Lattner](https://github.com/lattner)
* Status: **Waiting for scheduling** 
* Review manager: **TBD**

## Introduction

Function types in Swift use parentheses around their parameter list (aligning
with the function declaration syntax, as well as the syntax used to call a
function).  However, in the degenerate case of a single non-variadic, unlabeled
argument with no attributes, Swift allows the parentheses to be omitted.  For
example, these types:

```swift
(Int) -> Float
(String) -> Int
(T) -> U
```

May be written as:

```swift
Int -> Float
String -> Int
T -> U
```

While this saves some parentheses, it introduces some minor problems, is not
consistent with other parts of the Swift grammar, reduces consistency within
function types themselves, and offers no additional expressive capability (this
is just syntactic sugar).  This proposal suggests that we simply eliminate the
special case and require parentheses on all argument lists for function types.

Swift-evolution thread: [[pitch] Eliminate the "T1 -> T2" syntax, require "(T1) -> T2"](https://lists.swift.org/pipermail/swift-evolution/Week-of-Mon-20160411/014986.html)

## Motivation

Allowing this sugar introduces ambiguities in the language that require special
rules to disambiguate.  For example:

```swift
() -> Int           // Takes zero arguments, or takes one zero-argument parameter?
(Int, Float) -> Int // Takes two arguments, or takes one two-argument tuple?
```

This syntactic sugar reduces consistency with other parts of the language, since
declarations always require parentheses, and calls requires parentheses as well.
For example:

```swift
func f(a : Int) { ... } // ok
func f a : Int { ... }  // my eyes!
```

Finally, while it is straight-forward to remove this in Swift 3 (given the other
migration that will be necessary to move Swift 2 code to Swift 3), removing this
after Swift 3 will be much harder since we won't want to break code then.  It is
now or never.

## History

The original rationale aligned with the fact that we wanted to treat all
functions as taking a single parameter (which was often of tuple type) and
producing a single value (which was sometimes a tuple, in the case of void and
multiple return values).  However, we’ve long since moved on from that early
design point: there are a number of things that you can only do in a parameter
list now (varargs, default args, internal vs API labels, etc), we removed
implicit tuple splat, and the compiler has long ago stopped modeling function 
parameters this way.

## Proposed solution and Impact on existing code

Just require parentheses on function types.  The migrator will automatically
add them to existing code when moving from Swift 2 to Swift 3.

## Related questions

This proposal is very simple and well scoped, but in discussion, several
follow-on questions have asked about what precedent this sets - if we change 
this, then what else would align to it.  While we cannot predict the future of
where the Swift community will want to go, this section states the opinion of 
the author on these topics.

### Should function return types be parenthesized?

In my opinion, no.  Unlike arguments, there is no precedent already in Swift
that leads to the result type of functions being parenthesized (e.g. in
declarations).  The result of a function also does not have any of the magic and
complexity of parameter lists: it really is just a type.

Finally, in terms of ergonomics, the return type of a function is very commonly
written in code - almost every function and method has one.  In contrast, 
function types are very rarely written - typically only when writing higher
order functions.

### Should we require parentheses in closure expression parameter lists?

In my opinion, no.  Swift currently supports a number of syntactic shortcuts in
closure parameter lists, which are important for expressiveness of simple
functional algorithms.  For example, very few people write out this long-form
expression to sort an array of integers backward:

```swift
y = x.sorted { (lhs : Int, rhs : Int) -> Bool in rhs < lhs }
```

Many people use:

```swift
y = x.sorted { lhs, rhs in rhs < lhs }
```

Or they use the even shorter form of `{ $1 < $0 }`.

Some folks have asked
whether it would make sense to start requiring the parentheses around the
parameter lists for consistency with function types.  However, note that this is
structurally a different kind of syntactic sugar: you are allowed to elide the
parens even when you have multiple arguments, you are allowed to omit the return
type, you are allowed to omit the types, and you're even allowed to omit the
parameter list in its entirety.  Short of a complete rethink of closure syntax
(something that I'm not suggesting - I'm personally very happy with our 
closure syntax!), requiring parentheses here would not improve the language in an
apparent way.



