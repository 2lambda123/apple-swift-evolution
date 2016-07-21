# Remove the `Boolean` protocol

* Proposal: [SE-0109](0109-remove-boolean.md)
* Authors: [Anton Zhilin](https://github.com/Anton3), [Chris Lattner](https://github.com/lattner)
* Status: Accepted ([Rationale](http://thread.gmane.org/gmane.comp.lang.swift.evolution/23844))
* Review manager: [Doug Gregor](http://github.com/DougGregor)

## Introduction

For legacy and historical reasons Swift has supported a protocol named `Boolean`
for abstracting over different concrete Boolean types.  This causes problems
primarily because it is pointless and very confusing to newcomers to Swift: is
quite different than `Bool`, but shows up right next to it in documentation and 
code completion.  Once you know that it is something you don't want, you
constantly ignore it.  Boolean values are simple enough that we don't need a
protocol to abstract over multiple concrete implementations.

From a historical perspective, it was a very early solution to the bridging
challenge of `BOOL` (which comes in as `ObjCBool`).  In the time since then,
Swift has developed a number of more advanced ways to solve these sorts of
bridging problems, and `BOOL` is already bridged in automatically as `Bool` in
almost all cases.

Another pragmatic problem with the `Boolean` protocol is that it isn't used
consistently in APIs that take Boolean parameters: almost everything takes
`Bool` concretely.  This means that its supposed abstraction isn't useful.  The
only significant users are the unary `!`, and binary `&&` and `||` operators.

[Discussion thread](http://thread.gmane.org/gmane.comp.lang.swift.evolution/21559)

## Proposal

Remove `Boolean` protocol, and change the three operators that take it to
operate on `Bool` directly.

## Impact on existing code

This change is backwards incompatible, but extremely unlikely to break code in
practice.  As already mentioned, APIs that have parameters or return values of
type `BOOL` are automatically bridged in as `Bool`, so they will not see any
change at all.  The only significant place they appear in APIs are for the
`STOP` out parameters on certain Objective-C collection types, e.g.:

```swift
class NSArray : NSObject ... {
  func enumerateObjects(_ block: @noescape (AnyObject, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
```

which is used like this:

```swift
x.enumerateObjects { value, index, STOP in
  STOP.pointee = true
}
```

This continues to work even without the `Boolean` protocol because `ObjCBool`
still conforms to `BooleanLiteralConvertible`.

