# Modernizing Swift's Debugging Identifiers

* Proposal: [SE-0028](https://github.com/apple/swift-evolution/blob/master/proposals/0028-modernizing-debug-identifiers.md)
* Author(s): [Erica Sadun](http://github.com/erica)
* Status: Awaiting review
* Review manager: [Chris Lattner](https://github.com/lattner)

## Introduction

This proposal aims to eliminate Swift's use of "[screaming snake case](https://en.wikipedia.org/wiki/Snake_case)" like `__FILE__` and `__FUNCTION__` and replacing identifier instances with common [octothorpe-prefixed](https://en.wiktionary.org/wiki/octothorpe) [lower camel case](https://en.wikipedia.org/wiki/CamelCase) `#identifier` representations.

*The Swift-Evolution discussion of this topic took place in the "[Review] SE-0022: Referencing the Objective-C selector of a method" thread and then in its own "[Proposal] Eliminating Swift's Screaming Snake Case Identifiers" thread*

## Motivation

Swift offers built-in `__FILE__`, `__LINE__`, `__COLUMN__`, `__FUNCTION__`, and `__DSO_HANDLE__` identifiers. The first four expand to string and integer literals corresponding to a current location in source code. The last provides an `UnsafePointer` to the current dynamic shared object (.dylib or .so file). These features provide high utility for logging, both tracing execution and enabling developers to [capture error context](http://ericasadun.com/2015/08/27/capturing-context-swiftlang/).

The current identifiers owe their syntax to C's `__FILE__` and `__LINE__` macros. These are built into C's preprocessor and expanded before running the C-language parser. Swift's implementation differs from C's but offers similar functionality and, unfortunately, similar symbols. This proposal aims to break free of the historic chains of their unsightly screaming camel case, which look like boa constrictors [trying to digest](https://s-media-cache-ak0.pinimg.com/originals/59/ea/ee/59eaee788c31463b70e6e3d4fca5508f.jpg) fully swallowed keywords.

## Proposed solution

Using octothorpe-prefixed keywords offers several advantages:

* They match the existing `#available` keyword  (D. Gregor)
* They match SE-0022's already-accepted `#selector(...)` approach that reference a method's Objective-C selector (D. Gregor)
* They support targeted code completion (D. Gregor)
* They add a compiler-supported expression type that doesn't steal keywords, introducing a convention where `#` means "invoke compiler substitution logic here" (J. Rose)
* They'd provide short-term solutions for a yet-as-undesigned macro system  (D. Gregor)

## Detailed design

This proposal renames the following identifiers:

* `__FILE__` -> `#file`. 
* `__LINE__` -> `#line`
* `__COLUMN__` -> `#column`
* `__DSO_HANDLE__` -> `#dsoHandle`

These identifiers retain the magic behavior of the existing `__LINE__` features: in a normal expression context, they expand to the location at that point.  In a default argument context, they expand to the location of the caller. 

Additional points to be considered by the Swift team for inclusion:

* Adding `#filename` to avoid using `lastPathComponent` on `#file` references.
* Adopting a lower-case naming standard including `#dsoHandle` and a potential future `#sourceLocation`.
* Retaining `__FUNCTION__` to be renamed as `#function`.
* Introducing `#symbol`, (e.g. Swift.Dictionary.Init(x:Int,y:String)), which summarizes context including module, type, and function. A fully qualified symbol enables users to access exactly the information they desire. It should contain parameter type information to properly identify member overloads.


## Possible Future Extensions

[SR-198](https://bugs.swift.org/browse/SR-198) requested the coalescing of existing identifiers. A structured `#sourceLocation` identifier could be added as a follow-on if and when the Swift team decides to tackle a standardized source location type, which would provide individual field or keyword access.

In support of summaries, Remy Demerest writes, "[I] love the idea that source location would be one object that you can print to get the full story while still retaining the possibility to use each individual components as needed, which is probably the rarer case. I never find myself wanting only some of properties and usually don't include them simply because it takes longer to write the format properly, if I can get them all in one go it's certainly a win."

Should such a type be adopted, I'd recommend support for common output summary representations suitably differentiated for debug and release logging. Alternatively `#context`, `#releaseContext`, and `#debugContext` summaries could be added independently of the adoption of `#sourceLocation`.

## Implementation notes

The octothorpe-delineated `#line` identifier already exists in Swift for resetting line numbers. Constraining the current `#line` directive to be the first token after a newline would disambiguate use. Alternatively, the context `#line` identifier could be renamed `#lineNumber`.
