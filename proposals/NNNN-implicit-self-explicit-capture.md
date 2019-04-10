# Feature name

* Proposal: [SE-NNNN](NNNN-implicit-self-explicit-capture.md)
* Authors: [Frederick Kellison-Linn](https://github.com/jumhyn)
* Review Manager: TBD
* Status: **Awaiting Review**
* Implementation: [apple/swift#23934](https://github.com/apple/swift/pull/23934/)
* Bug: [SR-10218](https://bugs.swift.org/browse/SR-10218)
* Forum threads: [Discussion](https://forums.swift.org/t/review-capture-semantics-of-self/22017), [Pitch](https://forums.swift.org/t/allow-implicit-self-in-escaping-closures-when-self-is-explicitly-captured/22590)

## Introduction

Modify the rule that all uses of `self` in escaping closures must be explicit by allowing for implicit uses of `self` as long as `self` is explicitly declared in the capture list for the closure. This would allow the following to compile without error:

```swift
class Test {
    var x = 0
    func execute(_ work: @escaping () -> Void) {
        work()
    }
    func method() {
        execute { [self] in
            x += 1
        }
    }
}
```

## Motivation

In order to prevent users from inadvertently creating retain cycles, the Swift compiler today requires all uses of  `self` in escaping closures to be explicit. Attempting to reference a member `x` of `self` without the `self` keyword gives the error:

```
error: reference to property 'x' in closure requires explicit 'self.' to make capture semantics explicit
```

In codebases that choose to omit `self` where possible, this can result in a lot of unwanted noise, if many properties of `self` are used in a row:

```swift
execute {
    let foo = self.doFirstThing()
    performWork(with: self.bar)
    self.doSecondThing(with: foo)
    self.cleanup()
}
```

## Proposed solution

Allow the use of implicit `self` when it appears in the closure's capture list. The above code could then be written as:

```swift
execute { [self] in
    let foo = doFirstThing()
    performWork(with: bar)
    doSecondThing(with: foo)
    cleanup()
}
```

This change still forces code which captures `self` to be explicit about its intentions, but reduces the cost of that explicitness to a single declaration. With this change explicit capture of `self` would be one of *two* ways to get rid of the error, with the current method of adding `self.` to each property/method access (without adding `self` to the capture list) remaining as a viable option.

The compiler would also offer an additional fix-it when implicit `self` is used:

```swift
execute { // <- Fix it: insert '[self] in ' to capture `self` explicitly
    let foo = doFirstThing()
    performWork(with: bar)
    doSecondThing(with: foo)
    cleanup()
}
```

This will require different versions depending on whether there is a capture list already present ("insert '`self, `'"), whether there are explicit parameters ("insert '`[self]`'"), and whether the user the user has already captured a variable with the name `self` (in which case the fix-it would not be offered). Since empty capture lists are allowed (`{ [] in ... }`), the fix-it will cover this case too.

This new rule would only apply when `self` is captured directly, and with the name `self`. This includes captures of the form `[self = self]` but would still not permit implicit `self` if the capture were `[y = self]` or if the user wrote something like:

```
func method() {
    let y = self
    execute { [self = y]
        x += 1 // error: 
    }
}
```

## Detailed design

The bulk of the work is done in `UnqualifiedLookupFactory::lookupNamesIntroducedByClosure`, where we check if the closure captures `self` explicitly, and if so, look through the capture into the type context of `self`. In order to do this, we need to be able to access any potential capture list from the `ClosureExpr`, so we add a backedge from `ClosureExpr` to `CaptureListExpr`.

The other significant portion of the implementation is the diagnostic logic in `diagnoseImplicitSelfUseInClosure`. This ends up being fairly complex because of the many different cases that we have to handle when inserting `self` into a (possibly non-existent) capture list. In order to support this diagnostic, some additional metadata (`getBracketRange`) is added to `ClosureExpr`.

## Source compatibility

This proposal makes previously illegal syntax legal, and has no effect on source compatibility.

## Effect on ABI stability

This is an entirely frontend change and has no effect on ABI stability.

## Effect on API resilience

This is not an API-level change, and has no effect on API resilience.

## Alternatives considered

### Always require `self` in the capture list

The rule requiring the use of explicit `self` is helpful when the code base does not already require `self` to be used on all instance accesses. However, many code bases have linters or style guides that require `self` to be used explicitly always, making the capture semantics opaque. Always requiring `self` to be captured in the capture list explicitly would ensure that there are no `self` captures that the programmer is unaware of, even if they naturally use `self` for instance accesses. This would be a more drastic, source breaking change (and is not ruled out by adopting this change), so it was not seriously pursued as part of this proposal.

### Eliminate the former fix-it

A less extreme solution to the problem described above is to simply stop offering the current fix-it that suggests adding the explicit `self.` at the point of reference in favor of only recommending the explicit capture list fix-it, when possible.


