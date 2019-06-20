# Synthesized `Comparable` conformance for simple `enum` types

* Proposal: **SE-0000**
* Authors: **Kelvin Ma** (*[taylorswift](https://forums.swift.org/u/taylorswift/summary)*)
* Review manager:
* Status: **awaiting implementation**

## introduction

[SE-185](https://forums.swift.org/u/taylorswift/summary) introduced synthesized, opt-in `Equatable` and `Hashable` conformances for eligible types. Their sister protocol `Comparable` was left out at the time, since it was less obvious what types ought to be eligible for a synthesized `Comparable` conformance and where a comparison order might be derived from. This proposal seeks to allow users to opt-in to synthesized `Comparable` conformances for simple `enum` types (`enum`s without raw or associated values), a class of types which I believe make excellent candidates for this feature. The synthesized comparison order would be based on the declaration order of the `enum` cases.

## motivation

Oftentimes, you want to define an `enum` where the cases have an obvious semantic ordering:

```swift
enum Membership
{
    case premium    // <
    case preferred  // <
    case general
}
```
```swift
enum Brightness
{
   case low         // <
   case medium      // <
   case high
}
```

However, implementing it requires a lot of boilerplate code which is error-prone to write and difficult to maintain. Some commonly used workarounds include:

* Declaring a raw `enum`, with an `Int` backing, and implementing the comparison using `self.rawValue`. This has the downside of associating and exposing a meaningless numeric value on your `enum` API, as well as requiring a copy-and-paste `<` implementation. Such an `enum` would also receive the built-in `init(rawValue:)` initializer, which may be unwanted.

```swift
enum Membership:Int, Comparable
{
    case premium
    case preferred
    case general

    static
    func < (lhs:Self, rhs:Self) -> Bool
    {
        return lhs.rawValue < rhs.rawValue
    }
}
```

* Manually implementing the `<` operator with a private `minimum(_:_:)` helper function. This is the “proper” implementation, but is fairly verbose and error-prone to write, and does not scale well with more enumeration cases.

```swift
enum Brightness:Comparable
{
    case low
    case medium
    case high

    private static
    func minimum(_ lhs:Self, _ rhs:Self) -> Self
    {
        switch (lhs, rhs)
        {
        case (.low,    _), (_, .low   ):
            return .low
        case (.medium, _), (_, .medium):
            return .medium
        case (.high,   _), (_, .high  ):
            return .high
        }
    }

    static func < (lhs:Self, rhs:Self) -> Bool
    {
        return (lhs != rhs) && (lhs == Self.minimum(lhs, rhs))
    }
}
```

As the second workaround is non-obvious to many, users also often attempt to implement “private” integer values for enumeration cases by manually numbering them. Needless to say, this approach scales very poorly, and incurs a high code maintenance cost as simple tasks like adding a new enumeration case require manually re-numbering all the other cases. Workarounds for the workaround, such as numbering by tens (to “make room” for future cases) or using `Double` as the key type (to allow [numbering “on halves”](https://youtu.be/KWcxgrg4eQI?t=113)) reflect poorly on the language.

```swift
enum Membership:Comparable
{
    case premium
    case preferred
    case general

    private
    var comparisonValue:Int
    {
        switch self
        {
        case .premium:
            return 0
        case .preferred:
            return 1
        case .general:
            return 2
        }
    }

    static
    func < (lhs:Self, rhs:Self) -> Bool
    {
        return lhs.comparisonValue < rhs.comparisonValue
    }
}
```

## proposed solution

Enumeration types which opt-in to a synthesized `Comparable` conformance would compare according to case declaration order, with later cases comparing greater than earlier cases. Only pure `enum` types, without raw or associated values, would be eligible for synthesized conformances.

While basing behavior off of declaration order is unusual for Swift, as we generally hew to the “all fields are reorderable by the compiler” principle, it is not a foreign concept to `enums`. For example, reordering cases in a numeric-backed raw `enum` already changes its runtime behavior, since the case declaration order is taken to be meaningful in that context. I also believe that `enum` cases and `struct`/`class` fields are sufficiently distinct concepts that making enumeration case order meaningful would not make the language incoherent.

Later cases will compare greater than earlier cases, as Swift generally views sort orders to be “ascending” by default. It also harmonizes with the traditional C/C++ paradigm where a sequence of enumeration cases is merely a sequence of incremented integer values.

## detailed design

Synthesized `Comparable` conformances will work exactly the same as synthesized `Equatable`, `Hashable`, and `Codable` conformances today. A conformance will not be synthesized if a type already provides an explicit `<` implementation.

```swift
enum Membership:Comparable
{
    case premium
    case preferred
    case general
}

([.preferred, .general, .premium] as [Membership]).sorted()
// [Membership.premium, Membership.preferred, Membership.general]
```

## source compatibility

This feature is strictly additive.

## ABI compatibility

This feature does not affect the ABI.

## API stability

This feature does not affect the standard library.

## alternatives considered

* Basing comparison order off of raw values or `RawRepresentable`. This alternative is inapplicable, as enumerations with “raw” representations don’t always have an obvious sort order anyway. Raw `String` backings are also commonly (ab)used for debugging and logging purposes making them a poor source of intent for a comparison-order definition.

```swift
enum Month:String, Comparable
{
    case january
    case february
    case march
    case april
    ...
}
// do we compare alphabetically or by declaration order?
```
