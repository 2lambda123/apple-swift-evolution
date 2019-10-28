# Standard Library Preview Package

* Proposal: [SE-0264](0264-stdlib-preview-package.md)
* Authors: [Ben Cohen](https://github.com/airspeedswift), [Max Moiseev](https://github.com/moiseev), [Nate Cook](https://github.com/natecook1000)
* Review Manager: [Dave Abrahams](https://github.com/dabrahams)
* Status: **Returned for Revision**
* Pitch Discussion: [Pitch: Standard Library Preview Package](https://forums.swift.org/t/pitch-standard-library-preview-package/27202)
* Decision Notes: [Returned for Revision](https://forums.swift.org/t/returned-for-revision-se-0264-standard-library-preview-package/29865)

## Introduction

We propose the addition of a new package, `SwiftPreview`, which will form the initial landing spot for certain additions to the Swift standard library.

Adding this package serves the goal of allowing for rapid adoption of new standard library features, enabling sooner real-world feedback, and allowing for an initial period of time where that feedback can lead to source- and ABI-breaking changes if needed.

As a secondary benefit, it will reduce technical challenges for new community members implementing new features in the standard library.

In the first iteration, this package will take the following:

- free functions and methods, subscripts, and computed properties via extensions, that do not require access to the internal implementation of standard library types and that could reasonably be emitted into the client
- new types (for example, a sorted dictionary, or useful property wrapper implementations)
- new protocols, with conformance of existing library types to those protocols as appropriate

The package will not include features that need to be matched with language changes, nor functions that cannot practically be implemented without access to existing type internals or other non-public features such as LLVM builtins.

For the purposes of this document, we will refer to the proposed standard library preview package as "the package" and the standard library that ships with the toolchain as "the library".

## Motivation

### Facilitating Rapid Adoption and Feedback

It is common for a feature proposed and accepted through Swift Evolution and added to the standard library to wait for months before it gets any real life usage. Take [SE-0199](https://github.com/apple/swift-evolution/blob/master/proposals/0199-bool-toggle.md) (the introduction of `toggle`) for example. It was [accepted](https://forums.swift.org/t/accepted-se-199-add-toggle-to-bool/10681) almost 6 months before Swift 4.2 was released.

Even though all additions go through a thorough review, there is no substitute for feedback from real-world usage in production. Sometimes we discover that the API is not quite as good as it might have been.

While the toolchains downloadable from swift.org are useful for experimentation, they cannot be used to ship applications, so are rarely used for much more than "kicking the tires" on a feature. The beta period for Xcode provides slightly more real usage, but is still relatively short, with feedback often coming too late to be applied, given that any changes would require a further review on Swift evolution as well as time to integrate into the converging release. And the nature of standard library additions are such that you do not always have an immediate need early in the beta to try out a feature such as partial sort or a min heap.

Once a feature ships as part of a Swift release, any future changes resulting from feedback from real usage must clear the very high bar of source and ABI stability. Even if the change is merited and a source break is justified, the absolute need for ABI stability can rule out certain changes entirely on technical grounds. Furthermore, for performance reasons, standard library types that rely on specialization need to expose some of their internal implementation as part of their ABI, closing off future optimizations or performance fixes that are only discovered through subsequent usage and feedback.

### Technical Challenges for Contributors

The requirements for contributing to the standard library can be prohibitive. Not everybody has the time and resources to build a whole stack including LLVM, Clang, and the Swift compiler itself just to change a part of the standard library. Additionally, Xcode and XCTest cannot easily be used to maintain and test standard library code.

Integrating changes into the standard library requires knowledge of non-public features and idioms relating to ABI-stability. In particular, implementing a performant, specializable, ABI-stable collection type while preserving partial future flexibility is _significantly_ harder than writing one that is only source-stable. Harder still is knowing what internal implementation details can be changed after such a partially-transparent type has been published as ABI.

It would be of great benefit to the community to allow proposals and contributions to the standard library without these requirements, leaving integration of the final ABI-stable version to a later date and possibly other contributors with more experience maintaining ABI-stable code.

## Proposed solution

We propose the introduction of a `SwiftPreview` SwiftPM package. The package will be a standalone project hosted on GitHub under the Apple organization, and will be part of the overall Swift project.

Whenever possible, proposals for additions to the standard library will land first in the package before migrating to the standard library. A PR against the preview package will be sufficient to fulfill the implementation requirement for an evolution proposal. Proposals that are accepted will land in the package immediately, and then be integrated into the standard library.

All additions to the standard library will **continue to use the Swift Evolution process**, no matter whether they first land in the package or not. All additions to the package should be made with the understanding that they will migrate to the ABI-stable standard library module that ships as part of the Swift toolchain.

If usage after acceptance of a proposal reveals unanticipated problems, a follow-up proposal amendments will be able to make source-breaking changes. Unlike the very high bar for source-breaking changes, and the absolute rules around ABI stability, the bar for changes to API that haven't shipped as part of a Swift release will be that of any other evolution proposal of a new API.

Starting life in the package will not be mandatory. Proposed additions can instead go straight into the standard library, if they do not meet the criteria for suitable package additions. See the detailed design section for an expanded discussion of these criteria.

Since the package will not be ABI-stable, it will not ship as a binary on any platform, or be a dependency of any ABI-stable package. This allows for changes to the internal implementation of any type, and the change/removal of any function, as part of implementation changes, follow-on proposal amendments, or subsequent proposals.

## Detailed design

The following additive evolution proposals could be made using the preview package:

- New algorithms implemented as extensions on library protocols
- Utility types (e.g. wrapper types returned from functions like the `Lazy` types underlying `.lazy`)
- New protocols, including conformances by standard library types
- New collection types
- Property wrappers, such as a late-initialized wrapper

The following are examples of changes that would _not_ go into the package:

- Types introduced as part of and inseparable from language features (like `Optional` or `Error`)
- Implementations that rely on builtins for a reasonable implementation (like atomics or SIMD types)
- Implementations that require access to other types internals to be performant
- Changes that can't be done via a package, like adding customization points to protocols

Some of these cases will require a judgement call. For example, making an extension method a customization point may bring a minor performance optimization — and so not prevent initial deployment in the package — or it may be a major part of the implementation, making the difference between an `O(1)` and `O(n)` implementation. Whether a proposal should go into the preview package should be part of the evolution pitch and review discussions.

### Evolution

The introduction of the `SwiftPreview` package does not change much in the process of Swift Evolution. Changes to the standard library API surface should go through a well-developed pitch - discussion - implementation - proposal - decision life-cycle.

The main difference from the existing process is that the final result will become available for general use immediately in a new version of the preview package. As users have real-world experience with the accepted functionality, any proposed amendments to the proposal must go through the same evolution process as an original proposal.

To provide time for feedback from real-world use of new additions, the Swift Evolution process should favor landing new features in a window immediately after branching for a major release. This doesn’t mean that important and valuable proposals can’t be added at different times, but they’ll be subject to increased scrutiny.

No proposal should be accepted into the package on a provisional basis: Reviewers should assume that every proposal will be migrated as-is unless feedback reveals that it was a clear misadventure. The review manager for any revising proposals will be responsible for ensuring that the review discussion focuses on feedback from real-world use, and not relitigation of previously settled decisions made during the original review.

### Migration

An important aspect of the design is the experience of users of the package when features that have been available from the package become available in the standard library in a new Swift or platform release. This is handled differently for functions and types.

#### Types

The preview package will have a different module name (`SwiftPreview`) to that of the standard library (`Swift`). As such, every type it declares will be distinct from the type once it migrates into the standard library, and can co-exist with it. Because the preview package is "above" the `Swift` module in the stack, users of the package will get the package version of the type by default in source files that have imported the preview package. They will be able to specify the library version instead by prepending `Swift.` to the type name. This has the benefit that addition of the type into the library is source-compatible with code already using the package version.

It does have a downside for code size. Package users do not benefit from the code-size wins of not including the new type in their app and instead using a version in the OS (though, given that most types in the standard library are generic and need to be specialized, this is not as big a problem as it might be). It would also mean package users miss out on optimizations that may be possible with the library implementation. Once inside the library, fast paths could benefit from internals of other types. For example, a ring buffer might be implemented in terms of the same storage as an `Array`, and conversion from one type to another could just be a question of copying a reference to the other's storage.

In these cases where the standard library's version of the type would be better, the user can easily switch to it by adding a `typealias TheType = Swift.TheType` to their code, or by prefixing the module on individual declarations if needed.

#### Functions

Unlike types, methods cannot be disambiguated. That is, you cannot write something like `myCollection.SwiftPreview.partialSort`. Swift does not have this feature yet (and while desirable, it should not be a dependency of this proposal). So there is no way similar to the typealias approach above to prefer the standard library's implementation of a protocol extension over the packages.

Since Swift 5.1, the standard library has had an internal feature that allows it to force-emit the body of a function into the client code, as a way of  back-deploying critical bug fixes. This `@_alwaysEmitIntoClient` attribute can be used from within the standard library to deploy functions only to prior platforms, at the cost of binary size of the client (when they use them). Again, this cost is mitigated in the case of protocol extensions by the fact that the specialized implementation is already inlined.

This allows use of `#if compiler` directives to simultaneously obsolete implementations in the package for the latest version of Swift when introducing new functions into the standard library. As long as the new library definitions are marked as `@_alwaysEmitIntoClient` the source compatibility of this should not be noticeable.

#### Type/Function Combinations

A common pattern used in the standard library is to return a type from a function, with the type driving much of the functionality initiated by the function call. For example, `myArray.lazy.map` returns a `LazySequence` which maps elements on the fly using the supplied closure.

Since types cannot be emitted into client code, these combination function/type features will follow the pattern for types. This will mean that the package version will be used by default, and type context will be needed to force a call to the standard library if desired.

#### Retirement from the Package

In order to keep the package size and maintainability manageable, implementations will be removed from the package in a version update **1 year** after migration to the standard library. This is a necessary balance between maintainability and convenience. Since the package is open-source, it should be possible to copy source for a specific feature if a user badly needs to keep it while also wanting to upgrade to the latest version of the package _without_ upgrading to the latest compiler.

### Testing

The standard library currently uses a combination of `StdlibUnittest` tests and raw `lit` tests. This works well, but is unfamiliar to most developers.

The preferred style of test for Swift packages is `XCTest`, and the package should adopt this approach. This has the benefit of familiarity, and integrates well with Xcode. The work to convert tests from this format to lit can be done at the point of migration into the library.

Certain features of `StdlibUnittest` – in particular crashLater and `StdlibCollectionUnittest`'s `checkCollection`, as well as the helper types like the minimal collections – should be ported over to `XCTest` if possible. This would make for an excellent starter bug, or could be done as part of introducing the first full-featured collection to the package that had a significant need for this kind of testing, but are not a requirement for accepting this proposal.

### GYB

There will be no use of [gyb](https://github.com/apple/swift/blob/master/utils/gyb.py) in the package. The need for gyb has been gradually reducing over time as language features like conditional conformance and improved optimization eliminated the need for it. Gyb is a powerful and useful tool, but it can cause code that is difficult to read and write, and does not interact well with Xcode, so runs counter one of the goals of this package: to simplify contribution.

### Versioning

The fundamental goals of the preview package are somewhat at odds with the usual goals of semantic versioning. Source-breaking changes, including retirements after promoting into the standard library, or source-breaking changes resulting from evolution proposals prior to promotion, will be common. Unlike many packages, the preview package will not converge over time, which normally reduces the frequency of a package's version bumps.

As such, the preview package will remain at major version `0` **permanently**. Minor version bumps will be able to include source-breaking changes. Patch updates should not break source and will just be used for bug fixes.

The use of a `0` major version at all times will act as a signifier that adopters need to be comfortable with the requirement to continuously update to the latest version of the package. It should not be taken as an indication that the package shouldn't be used for real-world code: the code should be considered production-worthy. But it is a preview, and not source stable. Where practical, deprecated shims could be used to preserve source compatibility between versions, but this may not always be practical.

The decision when to tag minor versions will be made by the release manager for the standard library as chosen by the Swift project lead.

### Source compatibility

None on Swift itself. The intention of the package is to facilitate long-term source stability by detecting issues with APIs prior to their integration into the library.

## Effect on ABI stability

None. The package will not be declared ABI stable. ABI stability will only happen when proposals are migrated to the standard library. This means that the preview package may not be used within binary frameworks.

Because we do not have cross-module optimization, package implementations should make use of `@inlinable` annotations. However, these annotations should be in the context of source stability only and should be fully reevaluated by the library integrator when stabilizing the ABI.

## Alternatives considered

### Additional Review Before Library Integration

A previous version of this proposal prescribed a window of time before promoting accepted features into the standard library, to provide additional bake time and an additional review point for the feature. However, this approach delays some important feedback that can only be gained from integration into the standard library, where overload resolution and performance can have different outcomes than the same APIs shipped in a package.

### No Semantic Versioning

An alternative to keeping the semver major version at `0` is to not version the package at all. With this approach, the package could only be included by packages by specifying a branch-based dependency. However, this would be too restricting at this time, as a branch-based dependency can only be imported by other branch-based dependencies. This would effectively limit use of the `SwiftPreview` package to top-level applications only.


