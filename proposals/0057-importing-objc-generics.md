# Importing Objective-C Lightweight Generics

* Proposal: [SE-0057](https://github.com/apple/swift-evolution/blob/master/proposals/0057-importing-objc-generics.md)
* Author: [Doug Gregor](https://github.com/DougGregor)
* Status: **Accepted for Swift 3**
* Review manager: [Chris Lattner](https://github.com/lattner)

## Introduction

Objective-C's *lightweight generics* feature allows Objective-C
classes to be parameterized on the types they work with, similarly to
Swift's generics syntax. Their adoption in Foundation's collection
classes allow Objective-C APIs to be bridged more effectively into
Swift. For example, an `NSArray<NSString *> *` bridges to `[String]`
rather than the far-weaker `[AnyObject]`. However, parameterized
Objective-C classes lose their type parameters when they are imported
into Swift, so uses of type parameters outside of bridged, typed
collections (`NSArray`, `NSDictionary`, `NSSet`) don't benefit in
Swift. This proposal introduces a way to import the type parameters of
Objective-C classes into Swift.

Swift-evolution thread: [here](http://thread.gmane.org/gmane.comp.lang.swift.evolution/2886)

## Motivation

Cocoa and Cocoa Touch include a number of APIs that have adopted
Objective-C lightweight generics to improve static type safety and
expressiveness. However, because the type parameters are lost when
these APIs are imported into Swift, they are effectively *less* type
safe in Swift than in Objective-C, a situation we clearly cannot
abide. This proposal aims to improve the projection of these
Objective-C APIs in Swift.

## Proposed solution

A parameterized class written in Objective-C will be imported into
Swift as a generic class with the same number of type parameters. The
bounds on the type parameters in Objective-C will be translated into
requirements on the generic type parameters in Swift:

* The generic type parameters in Swift will always be class-bound,
  i.e., the generic class will have the requirement `T : AnyObject`.
* If the bound includes a class type (e.g., `T : NSValue *` in
  Objective-C), the generic Swift class will have the corresponding
  superclass requirement (`T : NSValue`).
* If the bound includes protocol qualification (e.g., `T :
  id<NSCopying>` in Objective-C), each protocol bound is turned into
  a conformance requirement (`T : NSCopying`) on the generic Swift
  class.

The following Objective-C code:

    @interface MySet<T : id<NSCopying>> : NSObject
    -(MySet<T> *)unionWithSet:(MySet<T> *)otherSet;
    @end

    @interface MySomething : NSObject
    - (MySet<NSValue *> *)valueSet;
    @end

will be imported as:

```swift
class MySet<T : NSCopying> : NSObject {
  func unionWithSet(otherSet: MySet<T>) -> MySet<T>
}

class MySomething : NSObject {
  func valueSet() -> MySet<NSValue>
}
```

### Importing unspecialized types
When importing an unspecialized Objective-C type into Swift, we
will substitute the bounds for the type arguments. For example:

```
@interface MySomething (ObjectSet)
- (MySet *)objectSet;    // note: no type arguments to MySet
@end
```

will be imported as:

```swift
extension MySomething {
  func objectSet() -> MySet<NSCopying> // note: uses the type bound
}
```

### Restrictions on uses of Objective-C parameterized classes

While the Swift and Objective-C generics systems look similar on the
surface, they use fundamentally different semantic
models. Specifically, Objective-C lightweight generics are based on
*type erasure*, so we cannot in general recover the type arguments
from the metaclass of an Objective-C parameterized class (i.e.,
because `MySet`, `MySet<NSString *>`, and `MySet<NSNumber *>` all
share a metaclass). This leads to several restrictions:

* Downcasting to an instance or metatype of a parameterized
  Objective-C class is inherently uncheckable, so we place limits on
  such casts. For example,

  ```swift
  let obj: AnyObject = ...
  if let set1 = obj as? MySet<NSCopying> {
    // okay: every MySet is a MySet<NSCopying> by construction, so
    // we're just checking that this is a 'MySet'.
  }

  if let set2 = obj as? MySet<NSNumber> {
    // error: conditional cast to specialized Objective-C instance
    // doesn't check type argument 'NSNumber'
  }

  let set3 = obj as! MySet<NSNumber> // okay: we assert that it is safe

  if let set4 = obj as? MySet<NSCopying> {
    let set5 = set4 as! MySet<NSNumber> // here's how to get a MySet<NSNumber>
  }
  ```

* By default, extensions of parameterized Objective-C classes cannot reference the
  type parameters in any way. For example:

  ```swift
  extension MySet {
    func someNewMethod(x: T) { ... } // error: cannot use `T`.
  }
  ```
  
### Opting in to type argument discovery

Some Objective-C parameterized classes do carry information about
their type arguments. When this is the case, it is possible to lift
some of the restrictions described in the above section. There are two
distinct cases:

* *Abstract parameterized classes whose concrete subclasses are not
   parameterized*:
   [`NSLayoutAnchor`](https://developer.apple.com/library/mac/documentation/AppKit/Reference/NSLayoutAnchor_ClassReference/index.html#//apple_ref/occ/cl/NSLayoutAnchor)
   is one such example: it is parameterized on the anchor type, but
   there is a fixed set of such anchor types that are represented by
   subclasses: `NSLayoutXAxisAnchor` subclasses
   `NSLayoutAnchor<NSLayoutXAxisAnchor *>`, `NSLayoutDimension`
   subclasses `NSLayoutAnchor<NSLayoutDimension *>`, etc. Therefore,
   the type arguments can be recovered by looking at the actual
   metaclass.

* *Parameterized classes that store their type arguments in
   instances*:
   [`GKComponentSystem`](https://developer.apple.com/library/ios/documentation/GameplayKit/Reference/GKComponentSystem_Class/)
   is one such example: it is parameterized on the component type it
   stores, but it's initializer (`-initWithComponentClass:`) requires
   one to pass the component type's metaclass. Therefore, every
   *instance* of `GKComponentSystem` knows its type arguments.

A parameterized Objective-C class can opt in to providing information
about its type argument by implementing a method
`classForGenericArgumentAtIndex:` either as a class method (for the first case
described above) or as an instance method (for the second case
described above). The method returns the metaclass for the type
argument at the given, zero-based index.

For example, `NSLayoutAnchor` would provide a class method
`classForGenericArgumentAtIndex:` that must be implemented by each of its
subclasses:

    @interface NSLayoutAnchor<AnchorType> (SwiftSupport)
    /// Note: must be implemented by each subclass
    +(nonnull Class)classForGenericArgumentAtIndex:(NSUInteger)index;
    @end

    @implementation NSLayoutAnchor
    +(nonnull Class)classForGenericArgumentAtIndex:(NSUInteger)index {
      NSAssert(false, @"subclass must override +classForGenericArgumentAtIndex:");
    }
    @end

    @implementation NSLayoutXAxisAnchor (SwiftSupport)
    +(nonnull Class)classForGenericArgumentAtIndex:(NSUInteger)index {
      return [NSLayoutXAxisAnchor class];
    }
    @end

    @implementation NSLayoutYAxisAnchor (SwiftSupport)
    +(nonnull Class)classForGenericArgumentAtIndex:(NSUInteger)index {
      return [NSLayoutYAxisAnchor class];
    }
    @end

    @implementation NSLayoutDimension (SwiftSupport)
    +(nonnull Class)classForGenericArgumentAtIndex:(NSUInteger)index {
      return [NSLayoutDimension class];
    }
    @end

On the other hand, `GKComponentSystem` would implement an instance
method `classForGenericArgumentAtIndex:`:

    @interface GKComponentSystem<ComponentType : GKComponent *> (SwiftSupport)
    - (nonnull Class)classForGenericArgumentAtIndex:(NSUInteger)index;
    @end

    @implementation GKComponentSystem (SwiftSupport)
    - (nonnull Class)classForGenericArgumentAtIndex:(NSUInteger)index {
      return self.componentClass;
    }
    @end

Note that many parameterized Objective-C classes cannot provide either
of these methods, because they don't carry enough information in their
instances. For example, an `NSMutableArray` has no record of what the
element type of the array is intended to be.

However, when a parameterized class does provide this information, we
can lift some of the restrictions from the previous section:

* If the parameterized class provides an instance method
  `classForGenericArgumentAtIndex:`, the extension can use the type arguments
  in its instance methods, including accessors for instance properties
  and subscripts. For example:

  ```swift
  extension GKComponentSystem {
    var reversedComponents: [ComponentType] {
      return components.reversed()
    }

    static func notifyComponents(components: [ComponentType]) {
      // error: cannot use `ComponentType` in a static method
    }
  }
  ```

* If the parametized class provides a class method
  `classForGenericArgumentAtIndex:`, the extension can use type arguments
  anywhere.
 
  ```swift
  extension NSLayoutAnchor {
    func doSomething(x: AnchorType) { ... }
    class func doSomethingClassy(x: AnchorType) { ... }
  }
  ```

### Subclassing parameterized Objective-C classes from Swift

When subclassing a parameterized Objective-C class from Swift, the
Swift compiler will define `+classForGenericArgumentAtIndex:` and
`-classForGenericArgumentAtIndex:`. The Swift compiler has the
complete type metadata required, because it is stored in the (Swift)
type metadata, so these definitions will be correct. For example:

```swift
class Employee : NSObject { ... }

class EmployeeArray : NSMutableArray<Employee> {
  // +[EmployeeArray classForGenericArgumentAtIndex:] always returns
  // ObjC type metadata for Employee
}

class MyMutableArray<T : AnyObject> : NSMutableArray<T> {
  // +[MyMutableArray classForGenericArgumentAtIndex:] returns the
  // ObjC type metadata for T, extracted from the Swift metatype for
  // `self`.
}
```

## Impact on existing code

In Swift 2, parameterized Objective-C classes are imported as
non-parameterized classes. Importing them as parameterized classes
will break any existing references to the affecting APIs. There are a
handful of cases where type inference may paper over the problems:

```swift
let array: NSArray = ["hello", "world"] // okay, infer NSArray<NSString>
// old

var mutArray = NSMutableArray() // error: need type arguments for NSMutableArray
```

A migrator could introduce the type bounds as arguments, e.g.,
`NSArray` would get migrated to `NSArray<AnyObject>`. It is not the
best migration---many developers would likely want to tighten up the
bounds to improve their Swift code---but it should migrate existing
code.

## Alternatives considered

The only major alternative design involves bringing type erasure into
the Swift generics system as an alternative implementation. It would
lift the restrictions on extensions of parameterized Objective-C
classes by treating type parameters in such contexts as the type
bounds:

```swift
extension MySet {
  func someNewMethod(x: T) { ... } // okay: `T` is treated like `NSCopying`
}
```

Doing so could allow the use of "unspecialized" generic types within
Swift, e.g., `NSMutableArray` with no type bounds (possibly spelled
`NSMutableArray<*>`), which would more accurately represent
Objective-C semantics in Swift.

However, doing so comes at the cost of having two incompatible
generics models implemented in Swift, which produces both a high
conceptual burden as well as a high implementation cost. The proposed
solution implies less implementation cost and puts the limitations on
what one can express when working with parameterized Objective-C
classes without fundamentally changing the Swift model.
