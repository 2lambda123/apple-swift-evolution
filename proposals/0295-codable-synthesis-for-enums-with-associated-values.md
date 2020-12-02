# Codable synthesis for enums with associated values

* Proposal: [SE-0295](0295-codable-synthesis-for-enums-with-associated-values.md)
* Authors: [Dario Rexin](https://github.com/drexin)
* Review Manager: [Doug Gregor](https://github.com/DougGregor)
* Status: **Active Review (December 1...11, 2020)**
* Implementation: [apple/swift#34855](https://github.com/apple/swift/pull/34855)
* Pitch: [Forum Discussion](https://forums.swift.org/t/codable-synthesis-for-enums-with-associated-values/41493)

## Introduction

Codable was introduced in [SE-0166](https://github.com/apple/swift-evolution/blob/master/proposals/0166-swift-archival-serialization.md)
with support for synthesizing `Encodable` and `Decodable` conformance for
`class` and `struct` types, that only contain values that also conform
to the respective protocols.

This proposal will extend the support for auto-synthesis of these conformances
to enums with associated values.

## Motivation

Currently auto-synthesis only works for enums conforming to `RawRepresentable`.
There have been discussions about adding general support for enums in the past,
but the concrete structure of the encoded values was never agreed upon.
We believe that having a solution for this is an important quality of life
improvement.

## Proposed solution

### Structure of encoded enums

The following enum with associated values

```swift
enum Command: Codable {
  case load(key: String)
  case store(key: String, value: Int)
}
```

would be encoded to

```json
{
  "load": {
    "key": "MyKey"
  }
}
```

and 

```json
{
  "store": {
    "key": "MyKey",
    "value": 42
  }
}
```

The top-level container contains a single key that matches the name of the enum case,
which points to another container that contains the values as they would be encoded
for structs and classes.

Associated values can also be unlabeled, in which case they will be encoded into an
array instead (that needs to happen even if only one of the value does not have a label):

```swift
enum Command: Codable {
  case load(String)
  case store(String, value: Int)
}
```

would encoded to

```json
{
  "load": [
    "MyKey"
  ]
}
```

and 

```json
{
  "store": [
    "MyKey",
    42
  ]
}
```

An enum case without associated values would encode the same as one where all values have labels,
i.e.

```swift
enum Command: Codable {
  case dumpToDisk
}
```

would encode to:

```json
{
  "dumpToDisk": {}
}
```

This is done for compatibility reasons. If associated values are added to a case later on, the structure
would not change, unless those values are unlabeled.

With the exception of the last case, this solution is closely following the default behavior of the Rust library [serde](https://serde.rs/container-attrs.html).

### User customization

For the existing cases users can customize which properties are included in the encoded respresentation
and map the property name to a custom name for the encoded representation by providing a custom `CodingKeys`
declaration instead of having the compiler generate one. The same should apply to the enum case.
Given that enums are encoded into a nested structure, there are multiple `CodingKeys` declarations. One
that contains the keys for each of the enum cases, and one for each case that contain the keys for the
associated values.

**Example**

```swift
enum Command: Codable {
  case load(key: String)
  case store(key: String, value: Int)
}
```

Would have the compiler generate the following `CodingKeys` declarations:

```swift

// contains keys for all cases of the enum
enum CodingKeys: CodingKey {
  case load
  case store
}

// contains keys for all associated values of `case load`
enum CodingKeys_load: CodingKey {
  case key
}

// contains keys for all associated values of `case store`
enum CodingKeys_store: CodingKey {
  case key
  case value
}
```

Since cases with unlabeled parameters encode into unkeyed containers,
no `CodingKeys` enum will be generated for them.

Users can define custom `CodingKeys` declarations for all, or a subset
of the cases. If some of the cases in an enum should not be codable,
they can be excluded from the `CodingKeys` declaration.

**Example**

```swift
enum Command: Codable {
  case load(key: String)
  case store(key: String, value: Int)
  case dumpToDisk

  enum CodingKeys: CodingKey {
    case load
    case store
    // don't include `dumpToDisk`
  }
}
```

The compiler will now only synthesize the code for the `load` and `store`
cases. An attempt to en- or decode a `dumpToDisk` value will cause an error
to be thrown.

Customizing which values will be included follows the same rules as the
existing functionality. Values that are excluded must have a default value
defined if a `Decodable` conformance should be synthesized. If only `Encodable`
is synthesized, this restriction does not apply.

**Example**

```swift
enum Command: Codable {
  case load(key: String, someLocalInfo: Int)

  // invalid, because `someLocalInfo` has no default value
  enum CodingKeys_load: CodingKey {
    case key
  }
}
```

```swift
enum Command: Codable {
  case load(key: String, someLocalInfo: Int = 0)

  // valid, because `someLocalInfo` has a default value
  enum CodingKeys_load: CodingKey {
    case key
  }
}
```

```swift
enum Command: Codable {
  case load(key: String)

  // invalid, because `someUnknownKey` does not map to a parameter in `load`
  enum CodingKeys_load: CodingKey {
    case key
    case someUnknownKey
  }
}
```

Keys can be mapped to other names by conforming to `RawRepresentable`:

**Example**

```swift
enum Command: Codable {
  case load(key: String)

  enum CodingKeys: String, CodingKey {
    case load = "lade"
  }

  enum CodingKeys_load: String, CodingKey {
    case key = "schluessel"
  }
}
```

would encode to:

```json
{
  "lade": {
    "schluessel": "MyKey"
  }
}
```

## Source compatibility

Existing source is not affected by these changes.

## Effect on ABI stability

None

## Effect on API resilience

None

## Alternatives considered

Previous discussions in the forums have been considered, specifically separating
the discriminator and value into individual key/value pairs discussed in [this forum thread](https://forums.swift.org/t/automatic-codable-conformance-for-enums-with-associated-values-that-themselves-conform-to-codable/11499).
While we do believe that there is value in doing this, we think that the default
behavior should more closely follow the structure of the types that are encoded.
A future proposal could add customization options to change the structure to meet
individual requirements.
