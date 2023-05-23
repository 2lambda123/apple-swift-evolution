# Using C++ from Swift 

## Introduction

This document lays out a vision for the development of the "forward" half of C++ and Swift interoperability: using C++ APIs from Swift. It sets overarching goals that drive the project’s design decisions, outlines some high-level topics related to C++ interoperability, and, finally, investigates a collection of specific API patterns and potential ways for the compiler to import them. This vision is a sketch, rather than a final design for C++ and Swift interoperability. Towards the end, this document suggests a process for evolving C++ interoperability over time, and it lays out the path for finalizing the designs discussed here.

“Reverse” interoperability (using Swift APIs from C++) is another extremely important part of the interoperability story.  However, reverse interoperability has largely different goals and constraints, which necessarily mean a different design and therefore a different vision document. The [vision for reverse interoperability](https://github.com/apple/swift-evolution/blob/main/visions/using-swift-from-c%2B%2B.md) has already been [accepted](https://forums.swift.org/t/accepted-a-vision-for-using-swift-from-c/62102) by the Language Workgroup.

This document is a prospective feature vision document, as described in the [draft review management guidelines](https://github.com/rjmccall/swift-evolution/blob/057b2383102f34c3d0f5b257f82bba0f5b94683d/review_management.md#future-directions-and-roadmaps) of the Swift evolution process.  It has not yet been approved by the Language Workgroup.

## Goals

There are many reasons for programmers to use C++ from Swift. They might work mostly in Swift but need to take advantage of some code written in C++, anything from a small snippet to a large library. On the other end of the spectrum, they might be C++ programmers looking to adopt Swift as a memory-safe successor language, with a goal of gradually rewriting their codebases into Swift. The foremost goal of Swift's C++ interoperation is to work well for all of these use cases, removing barriers to writing Swift instead of C++, without compromising Swift as a language.

To do this, Swift must import C++ APIs safely and idiomatically. Swift's memory safety is a major feature of its design, and C++'s lack of safety is a major defect. If C++'s unsafety is fully inherited when using C++ APIs from Swift, interoperability will have made Swift a worse language, and it will have undermined one of the reasons to migrate to Swift in the first place. But Swift must also make C++ APIs feel natural to use and fit into Swift's strong language idioms. Often these goals coincide, because the better Swift understands how a C++ API is meant to be used, the more unsafety and boilerplate it can eliminate from use sites. If the Swift compiler does not understand how to import an API safely or idiomatically, it should decline to import it, requesting more information from the user (likely through the use of annotations) so that the API can be imported in a way that meets Swift’s standards.

For example, many C++ APIs traffic in iterators. Direct uses of C++ iterators are difficult to make safe: iterators are unsafe unless used correctly, and that correctness relies on complex properties (such as that the iterator has not been invalidated) that are hard to statically enforce given how iterators are expected to be used. Iterators are also not very idiomatic in Swift because iterator values can only be meaningfully interpreted in pairs. Swift should recognize common C++ patterns like ranges (pairs of iterators) and containers and map them into Swift `Collection`s, making them automatically work with Swift's library of safe and idiomatic collections algorithms. For example, Swift code should be able to filter and map the contents of a `std::vector`:

```Swift
images // "images" is of type std::vector<CxxImage>
  .filter { $0.size > 256 }
  .map(UIImage.init)
```

This level of idiomatic interoperation allows programmers to immediately see the benefits of adopting Swift, even when using C++ APIs. It makes the two languages work cleanly together. It removes the need for extensive C or Objective-C bridging layers between C++ libraries and their Swift clients, which are often the source of bugs, performance problems, and expressivity restrictions. And when combined with "reverse" interop that exposes Swift APIs to C++, it allows Swift to be added incrementally to an existing C++ codebase and interoperate on a file-by-file basis, enabling it to function as a viable successor language for programmers looking to move past C++.

Swift has had great success as a successor language to Objective-C with this approach of bidirectional, file-by-file interoperation. While the constraints and trade-offs of interoperating with C++ are vastly different from Objective-C, the same overall philosophy can largely be applied to make Swift an excellent C++ successor, because it permits the incremental adoption of Swift in a codebase rather than relying on all-at-once rewrites. To make this viable, interoperation must not rely on radically changing interfaces on either side of the language barrier. For Objective-C, Swift takes the approach of largely incorporating Objective-C by inclusion: most major Objective-C features have corresponding Swift features that are at least as expressive. This is not desirable for C++, most importantly because most of the unsafety of C++ arises from its widespread and idiomatic use of unmanaged pointer and reference types; naively translating these all to `UnsafePointer`s would create an unidiomatic and unsafe mess. So successful import of C++ to Swift must rely on recognizing patterns of how these features are used, perhaps with user guidance, and mapping them to more idiomatic Swift constructs. This idea has proven successful with Objective-C, such as methods with `NSError**` parameters being translated to `throws`; C++ will just need to use it more pervasively.

Because of this, importing C++ APIs into Swift is a difficult task that must be handled with care. Almost every goal of C++ interoperability will be in tension with Swift's safety requirements. Swift must strike a careful balance in order to maintain Swift's safety without reintroducing the development, performance, or expressivity costs of an intermediate wrapper API.

Safety is a top priority for the Swift programming language, which creates a tension with C++. While Swift enforces strong rules around things like memory safety, mutability, and nullability, C++ largely makes the programmer responsible for handling them correctly, on the pain of undefined behavior. Simply using C++ APIs should not completely undermine Swift's lanaguage guarantees, especially guarantees around safety. At a minimum, imported C++ APIs should generally not be less safe to use from Swift than they would be in C++, and C++ interoperability should strive to make imported APIs *safer* in Swift than they are in C++ by providing safe API interfaces for common, unsafe C++ API patterns (such as iterators). When it is possible for the Swift compiler to statically derive safety properties and API semantics (i.e., how to safely use an API in Swift) from the C++ API interface, C++ interoperability should take advantage of this information. When that is not possible, C++ interoperability should provide annotations to communicate the necessary information to use these APIs safely in Swift. When APIs cannot be used safely or need careful management, Swift should make that clear to the programmer. As a last resort, Swift should make an API unavailable if there's no reasonable path to a sufficiently safe Swift interface for it.

C++ interoperability should strive to have good diagnostics. Diagnostics that report source locations for a C++ API should refer to the API's original declaration in a C++ header, not to a location in a synthesized interface file. When a C++ API can be imported into Swift, diagnostics from misusing it (e.g. type errors when passing it an argument of the wrong type) should be similar to the diagnostics for analogous misuses of a Swift API. When a C++ API cannot be imported, attempts to use it should result in a clear error indicating why the API could not be imported, and the diagnostics should suggest specific ways that the programmer could make it importable (for example, by adding annotations).

C++ provides tools to create high-performance APIs. The Swift compiler should embrace this. Interop should not be a significant source of overhead, and performance concerns should not be a reason to continue using C++ to call C++ APIs rather than Swift.

C++ is an "unopinionated" multi-paradigm language, designed to fit many use cases and allow many different programming styles. Different codebases often express the same concept in different ways. There is no prevailing consensus among C++ programmers about the right way to express specific concepts: how to name types and methods, how much to use templates, when to use heap allocation, how to propagate and handle errors, and so on. This creates problems for importing C++ APIs into Swift, which tends to have stronger conventions, some of which are backed by language rules. For instance, it is a common pattern in some C++ codebases to have classes that are only (or at least mostly) intended to be heap-allocated and passed around by pointer; consider this example:

```
// StatefulObject has object identity and reference semantcs: 
// it should be constructed with "create" and used via a pointer.
struct StatefulObject {
  StatefulObject(const StatefulObject&) = delete;
  StatefulObject() = delete;

  StatefulObject *create() { return new StatefulObject(); }
};
```

This type is not intended to be used directly as the type of a local variable or a `std::vector` element. Values of the type are allocated on the heap by the `create` method and passed around as a pointer. This is weakly enforced by the way the type hides its constructors, but mostly it's communicated in the documentation and by the overall shape of the API. There is no C++ language feature or programming pattern that directly expresses these semantics. 

If `StatefulObject` were written idiomatically in Swift, it would be defined as a `class` to make it a reference type. This is an example of how Swift defines clear patterns for naming, generic programming, value categories, error handling, and so on, which codebases are encouraged to use as standard practices. These well-defined programming patterns make using Swift APIs a cohesive experience, and C++ interoperability should stive to maintain this experience for Swift programmers using C++ APIs.

To achieve that, the compiler should map C++ APIs to one of these specific Swift programming patterns. In cases where the most appropriate Swift pattern can be inferred by the Swift compiler, it should map the API automatically. Otherwise, Swift should ask programmers to annotate their C++ APIs to guide how they are imported. For example, Swift imports C++ types as structs with value semantics by default. Because `StatefulObject` cannot be copied, Swift cannot import it via the default approach. To be able to use `StatefulObject`, the user should annotate it as a reference type so that the compiler can import it as a Swift `class`. Information on how to import APIs, such as `StatefulObject`, cannot always be statically determined (for example, `StatefulObject` might have been a move-only type, a singleton, or RAII-style API). The Swift compiler should not import APIs like `StatefulObject` for which it does not have sufficent semantic information. It is not a goal to import every C++ API into Swift, especially without additional, required information to present the API in an idiomatic way that promotes a cohesive Swift expirence.

Because of the difference in idioms between the two languages, and because of the safety concerns when exposing certain APIs to Swift, a C++ API might look quite different in Swift than it does in C++. It is a goal of C++ interoperability to provide a clear, well-defined mapping for whether and how APIs are imported into Swift. Users should be able to read the C++ interoperability documentation to have a good idea of how much of their API will be able to imported and what it will look like. Swift should also provide tools for inspecting what a C++ API will look like in Swift, and these tools should call out notable parts of the API that were not imported.

## The approach

Many C++ constructs have a clear, analogous mapping in Swift. These constructs can be easily and automatically imported to their corresponding Swift constructs. For example: C++ operators can usually be mapped to similar Swift operators. Sometimes, to promote Swift’s idioms, operators should be imported semantically rather than directly. For example, `operator++` should map to a `successor` method in Swift and `operator*` should map to a `pointee` property to promote clarity and uphold Swift’s strong idioms. Another example is namespaces which can be mapped to empty Swift enums, a common pattern in Swift. And enums, even class enums, can be mapped trivially to Swift enums. 

Swift and C++ both support object-oriented programming, allowing objects and members to be expressed easily. For the most part, these objects and their members can be mapped trivially. A member function in C++ translates to a method in Swift, and so on. Swift and C++ not only support object-oriented programmer, but also promote value semantics, providing a clear default for importing C++ classes and structs. Swift can express C++ objects as `struct`s or “value types” where copying, moving, and destroying the value invokes one of the C++ type’s special members: copy constructor, move constructor, or destructor. This default for importing C++ types fits nicely into Swift’s existing object model, and allows most types to automatically work in Swift in an idiomatic way that feels native. 

Unfortunately, not all C++ types are value types, and while Swift expresses value categories explicitly, C++ does not. The example above (in the goals section) shows how the basic tools provided by C++ are often used in idiomatically different ways. This poses a conundrum for importing C++ APIs automatically into Swift which extends far past value categories. A more fundamental place to see this is with memory management.

In Objective-C, it's fairly straightforward for ARC to ensure that data is valid when it's used. Almost all data in Objective-C is represented with either a fundamental type (such as `double` or `BOOL`) or a reference-counted object type (such as `NSString *`). Values of fundamental types can be safely copied around, and reference-counted objects can be "managed" by retaining them, extending their lifetime without changing the semantics of the program. It's rare to work with an unmanaged pointer in Objective-C, and even rarer to work with an unmanaged pointer that has a dependency on a managed object (although exceptions do exist, such as [`NSData`'s `-bytes` method](https://developer.apple.com/documentation/foundation/nsdata/1410616-bytes?language=objc)). This kind of dependency is problematic for safe memory management because the language often does not know about it and cannot ensure that the backing object stays valid while the pointer is being used. Swift's Objective-C interop has treated these as special cases (with an attribute) and dealt with them individually; while this solution is imperfect in several ways, its rarity has made it a low priority to fix.

In contrast, it's very common for C++ APIs to work with unmanaged pointers, references, and views into other objects. The lifetime rules for using these correctly are inconsistent and sometimes unique to an API. As an example, consider three values: a value of type `std::vector<std::string>`, a reference returned from that vector's subscript operator, and an iterator returned from that vector's `begin()` method. At first glance, these values look similar to the compiler: they are all either pointers or class objects containing pointers. But each has its own semantics and expected use (especially concerning lifetime), and these differences are not conveyed explicitly in the source. The vector is a value type that can be copied, but copies can be expensive, and iterators and references into the vector are only valid for a specific copy. The result of the subscript operator is a mutable projection of a specific element, dependent on the vector for validity; but the value of that element can be copied out of the reference to get  an independent value, and that is often how the subscript operator is used. The iterator is also a projection, dependent on the vector for validity, but it must be used in conjunction with other iterators or with the vector itself in certain careful ways, and some operations will invalidate it completely.

So there is a conundrum where superficially similar language constructs in C++ are used to express idiomatic patterns that are vastly different in their impact. The only viable approach for addressing this conundrum is to pick off these patterns one at a time. The Swift compiler will know about many possible C++ API patterns. If a C++ API has semantic annotations telling Swift that it follows a certain pattern, Swift will try to create a Swift interface for it following the rules of that pattern. In the absence of those annotations, Swift will try to use huristics to recognize an appropriate pattern. If this fails, Swift will make the API unavailable.

Consider how this applies to the `std::vector` example. `std::vector<std::string>` maps over well as a Swift value type. Its subscript operator can be imported as a Swift `subscript`, and the importer can take advantage of the fact that it returns a reference to allow elements to be efficiently borrowed. And while C++ iterators in general pose serious lifetime safety problems in Swift, Swift can recognize the common `begin()`/`end()` pattern and import it as a safe Swift iterator that encapsulates the unsafety internally. The following sections will go into detail explaining how each of these specific API patterns can be picked off of a C++ codebase. 

### Importing types

One of the most common uses of this "API patterns" concept concerns the import of types. Swift types fall into two categories: value types and reference types. Copying a value of a reference type produces a new reference to the same underlying object, similar to an intrusive `std::shared_ptr` in C++ or a class type in Java. Copying a value of a value type recursively copies the components of the type to produce an independent value, similar to the behavior of a struct in C or the default behavior of a class type in C++. Furthermore, both kinds of types must always be copyable, although there are plans in the works to allow types to restrict this.

Types in C++ do not always fit cleanly into this model, and they cannot always be automatically mapped to it even when they do. Many C++ classes are meant to be used as value types, but there are also quite a few C++ classes that Swift programmers would think of as reference types. The difference is not necessarily obvious in source. The closest bit of information that C++ provides directly is whether and how a class has changed its value operations (its copy and move constructors, its assignment operators, and its destructor). A reference type is more likely to delete its copy operations, while a value type is likely to still provide them. But this is not a reliable signal, because some value types are meant to be uncopyable (or even unmovable), while some reference types leave their copy operations intact, either by neglect or to enable objects to be easily cloned when necessary. Furthermore, C++ types sometimes have a more hybrid semantics: iterators, for example, can be used like values, but they're not independent from their underlying collection and in some ways act like references. And some C++ types aren't meant to be used as normal values at all; instead they fill specific idiomatic purposes, like the proxy element references used by `std::vector<bool>`, or scoped-destructor types like `std::lock_guard`.

### Reference types

Reference types generally fit well into the existing Swift model, and there is little need to restrict them. The safety properties of managed reference types imported from C++ are generally similar to Swift's own class types and imported Objective-C class types. The design below also includes unmanaged reference types, which are less safe than managed types, but not more unsafe than writing the code in C++. Overall, this allows C++ interoperability to offer a clear, native-feeling mapping for several common C++ API patterns.

#### Criteria for importing as a reference type

Whether a C++ class type is appropriate to import as a reference type is a complex question, and there are several criteria that go into answering it.

The first criterion is whether object identity is part of the "value" of the type. Is comparing the address of two objects just asking whether they're stored at the same location, or it is deciding whether they represent the "same object" in a more significant sense? For example, consider a computer game that uses a world model where each object of the `GameObject` class represents a different game object. Copying an object actually means making a second object in the game world, one which initially shares the same internal data as another. This is a classic use of reference semantics, and `GameObject` is clearly a reference type. In contrast, a different game might use a world model where the `GameObjectState` class holds a snapshot of the current state of a game object. The actual game object is identified as part of that snapshot, but it's not synonymous with the snapshot, and copying the snapshot just produces an equivalent snapshot of the same object. This design does not rely on object identity; if `GameObjectState` is a reference type, it is because of some other factor.

The second criterion is whether the C++ class is polymorphic. Does the class have subclasses whose objects contain additional data or behave differently from objects of the parent class? Swift value types cannot be directly polymorphic, so if polymorphism is an important part of a C++ class, it must be imported as a reference type. The most common indicator of a polymorphic C++ class is having `virtual` methods. More rarely, some C++ classes behave polymorphically but intentionally avoid having `virtual` methods to eliminate the memory overhead of a v-table pointer in every object.

The third and final criterion whether objects of the C++ class are always passed around by reference.  Are objects predominantly passed around using a pointer or reference type, such as a raw pointer (`*`), raw reference (`&` or `&&`), or smart pointer (like `std::unique_ptr` or `std::shared_ptr`)?  When passed by raw pointer or reference, is there an expectation that that memory is stable and will continue to stay valid, or are receivers expected to copy the object if they need to keep the value alive independently?  If objects are generally allocated and remain at a stable address, even if that address is not semantically part of the "value" of an object, the class may be idiomatically a reference type. This will sometimes be a judgment call for the programmer.

Most of these criteria are not possible for a compiler to answer automatically by just looking at the code.  A compiler cannot know the semantic meaning of object identity for a class type.  Nor can can it know whether it is looking at a representative sample of how a type is passed around in a project.  Classes satisfying these criteria will have to be annotated somehow to tell the compiler to import them as Swift classes.  The one exception is that it might be reasonable to assume that a C++ class with `virtual` functions should be imported as a reference type.

#### Object management

Swift generally promises to make sure that objects are valid when used.  This is an important part of Swift's core language goal of memory safety.  Ideally, when Swift imports a C++ class as a reference type, it will import it as an appropriately managed type that receives the same guarantees as native Swift and imported Objective-C classes.

It's useful to split the object-management problem into two questions: how objects are managed and whether they can be managed automatically.

There are three common patterns for managing reference object lifetimes in C++.  Swift should endeavor to support all three of them:

  - **Immortal** reference types are not designed to be managed individually by the program. Objects of these types are allocated and then intentionally "leaked" without tracking their uses. Sometimes these objects are not truly immortal: for example, they may be arena-allocated, with an expectation that they will only be referenced from other objects within the arena. Nonetheless, they aren't expected to be individually managed.

    The only reasonable thing Swift can do with immortal reference types is import them as unmanaged classes.  This is perfectly fine when objects are truly immortal.  If the object is arena-allocated, this is unsafe, but it's essentially an unavoidable level of unsafety given the choices of the C++ API.

  - **Unique** reference types are owned by a single context at once, which must ultimately either destroy it or pass ownership of it to a different context. There are two common idioms for unique ownership in C++. The first is that the object is passed around using a raw pointer (or sometimes a reference) and eventually destroyed using the `delete` operator. The second is that this is automated using a move-only smart pointer such as `std::unique_ptr`. This kind of use of `std::unique_ptr` is often paired with "borrowed" uses that traffic in raw pointers temporarily extracted from the smart pointer; in particular, method calls on the class via `operator->` implicitly receive a raw pointer as `this`.

    The introduction of [non-copyable types](https://forums.swift.org/t/pitch-noncopyable-or-move-only-structs-and-enums/61903) will allow Swift to directly support unique reference types as managed types.  The main challenge in doing this will be understanding the ownership conventions for different C++ APIs.  If ownership of a class is known to be passed around with a smart pointer like `std::unique_ptr`, then APIs trafficking in raw pointers can be assumed to be working with a borrow; that would support importing as a managed type.  Otherwise, Swift will have to put the programmer in charge and either import as an unmanaged type or use a wrapper type like `Unmanaged` to mediate APIs with unknown conventions.

  - **Shared** reference types are reference-counted with custom retain and release operations. In C++, this is nearly always done with a smart pointer like `std::shared_ptr` rather than expecting programmers to manually use retain and release. This is generally compatible with being imported as a managed type. Shared pointer types are either "intrusive" or "non-intrusive", which unfortunately ends up being relevant to semantics. `std::shared_ptr` is a non-intrusive shared pointer, which supports pointers of any type without needing any cooperation.  Intrusive shared pointers require cooperation but support some additional operations. Swift should endeavor to support both.

    As with unique reference types, shared reference types in C++ often have APIs that take raw pointers, such as methods on the class type. Unlike unique references, these cannot necessarily be thought of as borrows. Shared reference types are copyable types, and as a general rule, borrowed copyable values can be copied to produce owned values. However, in C++ terms, this would require constructing a shared pointer value from a raw pointer, which in general is not possible to do correctly for non-intrusive shared pointers. It's fine for Swift to *call* APIs that take raw pointers for shared reference types, but it cannot *implement* them without having a way to prevent copying the reference.

    `std::shared_ptr` uses atomic reference-counting in both its intrusive and non-intrusive modes.  Non-atomic smart pointer types are supportable, but the imported class type must not be `Sendable`.
  
[Examples of each of these are given below.](## Examples and Definitions)

If a type is annotated as using one of these reference-type patterns, uses of the type in C++ that do not have a consistent interpretation under the pattern will be impossible to import.  For example, suppose that `GameObject` is annotated as a shared reference type that uses `std::shared_ptr`.  A C++ API that takes a parameter of type `std::unique_ptr<GameObject>`, or a different shared pointer class from the annotated one, must be made unavailable.  (This would also include differences in secondary template arguments, such as the `Deleter` template argument of `std::unique_ptr`.)  Similarly, a C++ API that takes a `GameObject` as an r-value must be made unavailable.

Swift doesn't have to force programmers to pick one of these patterns specifically. Without further information, foreign reference types can be imported as an unmanaged class type. There would be an operation on an object to delete it, but if that's not safe to use, the programmer could simply not use it. This behavior would allow types with reference semantics to be expressed with little effort at the cost of complete safety. For some types this may be acceptable or even necessary. However, Swift should strive to make it easy to import types as managed class types, especially for APIs that already make extensive use of smart pointers.

### Value types

Value types have value semantics, that is, they can be copied and destroyed. Each instance of the type is a separate copy of the object, rather than a reference to the underlying storage. Swift expresses value types using structs which have the same behavior as C struct. C++ structs also have this behavior by default, but can be custimized to have more complicated lifetime operations, often via custom copy constructors. Custom copy operations are often used to manage storage in C++. While value types with custom copy operations fit into the Swift value type model at a high level, these types are actually novel to Swift. In Swift there is no way to define a custom copy operation and managed storage ususually has long, stable lifetimes. This is in contrast to C++ value types which may have a short lifetime, where storage is associated with an individual copy of the object. To accomidate this novelty, value types must be broken down into three categories. These categories are largely opaque to users of interop, but are essential to describing the interop story, safety and performance properties, potential API restrictions, and the user model more generally.

#### Simple data types

This document will refer to C++'s trivially-copyable value types that do not hold pointers as “simple data types.” These types include primitive types such as integers and types which are composed of other simple data types. Simple data types are “owned” types that provide trivial lifetime operations: a copy is a copy of their bits and a destroy is a no-op. Simple data types have roughly the same mapping throughout Swift, C, Objective-C, and C++ making them trivial to import. Simple data types, their instances, methods on simple data types, and other APIs that use simple data types are generally considered to be safe and usable.

**View types**

This document will refer to trivially-copyable value types that hold pointers as “view types.” These types include pointers themselves and types which are composed of any other view types (potentially including other types as well). The pointers held by view types refer to memory that is *not owned* by the pointer type (making view types a “view” or “projection” into memory). While view types are very similar to simple data types with respect to their trivial lifetime operations and the fact that they map similarly in these four language, they differ in the fact that while they themselves are not inherently unsafe, they may be used in unsafe APIs (discussed later).

#### Self-contained types

This category of types subsumes trivial types to include types with non-trivial members and custom lifetime operations. These types might be "view types" except for the fact that self-contained types *do own* the memory that their members point to. C++ often uses copy constructors and destructors to manage the lifetime of self-contained types. Therefore, the Swift compiler should assume that view types with custom copy constructor and destructors own their memory. Unfortuantly, this is not always the case. Types like `std::vector<int *>` have these custom lifetime operations, but do not own their storage. For these cases, Swift must provide annotations that allow the default to be corrected.

### Other projections

Value types that own memory through custom lifetime operations do not natively exist in Swift today. Any value types that own memory in Swift do so transatively through reference types that have long, stable lifetimes. Because Swift was not built around this kind of value type with short lifetimes and deep copies, dealing with projections of these owned types can be dangerous. This pattern is, up until now, foreign to Swift, so there are no existing tools that allow users to control this behavior or improve saftey. The best model for handling these potentially unsafe APIs is unclear; maybe most projects can be represented using generalized accessors, maybe most projects can be represented as iterators, maybe some projections should not be projections at all (and rather imported as values that are copied), most likely the answer is some combination of these. The best approach for handling projections will be revealed over time as evolution posts propose potential solutions, such as the iterator bridging described below, and as users of interop provide feedback.

Besides the dissonant semantic models for representing projections, the bredth of ways to define projections in C++ will prove a challenege for importing this API pattern. 

Consider the following API which returns a vector of pointers:
```
std::vector<int *> OwnedType::projectsInternalStorage(); 
```

Or this API which fills in a pointer that has two levels of indirection:
```
void VectorLike::begin(int **out) { *out = data(); }
```

Or even this global function that projects one of it's parameters:
```
int *begin(std::vector<int> *v) { return v->data(); }
```

It may be convient for Swift to assume that all projects follow one, unique pattern: a method of an owned type that returns a pointer. However, that is certainly not the only way in which a projection can be created. This is but one of the many places where Swift will need to decide between expressiveness and safety. Allowing the above APIs to be imported would allow interop to be more usable by default. Taking the first example, most of the time, when a vector holds pointers, those pointers do not point to storage with a short lifetime. Making this API unavailable would ensure 100% safety on the pain usability in the 99% case, when this API is safe. The tradeoffs here are an open question for the Swift evolution process to eventually determine. In any case, it is essential that C++ interoperability makes certain assumptions about the APIs Swift imports. There will always be an edge case that cannot be covered or does not make sense to accomidate. Interop as a whole should not become unusable so that these edge cases can be accomidated, or worse yet, so that that neither safe nor unsafe APIs are available in Swift. So, C++ interoperability should make reasonable (not conservative) assumptions when importing APIs.

 ### Iterators

Both Swift and C++ have powerful libraries for algorithms and iterators. The standard C++ iterator API interface lends itself to the Swift model, allowing C++ iterators and ranges to be mapped to Swift iterators and sequences with relative ease. These mapped APIs are idomatic, native Swift iterators and sequences; their semantics match the rest of the Swift language and Swift APIs compose around them nicely. By taking on Swift iterator semantics, iterators that are imported in this way are able to side-step most or all of the issues that other projects have (described above). 

Swift's powerful suite of algorithms match and go beyond the standard library algorithms provided by C++. These algorithms compose on top of protocols such as Sequence, which C++ ranges should automatically conform to. These Swift APIs and algorithms that operate on Swift iterators and sequences should be prefered to their C++ analogous, as they fit into the rest of the language natrually. However, algorithms are not the only API which operate on iterators and sequences and other C++ APIs must still be useable from Swift. The best way to represent C++ APIs that take one or many iterators (potentially pointing at the same range) is not clear and will need to be explored during the evolution processes.

### Mutability

Mutability is a concept where Swift and C++ diverge: by default C++ values are mutable and even when a value is const in C++, it is easily and often ignored (cast away). C++ often overloads functionality on a value’s constness, harming code-clarity. The generally weak notion of mutability in C++ leads to few codebases fully and strictly utilizing the `const` keyword. Swift, on the other hand, enforces mutability strictly and by default. In Swift, mutability conveys semantics, so importing largely mutable C++ codebases can lead to confusion. Swift should encourage C++ codebases to adopt const on methods and values that are used in Swift and expect that C++ does not mutate values that are marked as `const`. And overloaded functions should be clearly be disambiguated in Swift through naming. Mutability is a place where programmers may need to intervene and provide Swift with more information to help promote idiomatic APIs that are expressive and feel natural in Swift. As discussed in the "Views" section, the Swift compiler must make assumptions about the C++ APIs that it is importing, and mutability is another place where Swift will need to make reasonable (not conservative) assumptions about the APIs that it is importing, promoting C++‘s weak notion of `const` to Swift’s much stricter ideal. 

By using Swift's strong notion of mutability, programmers will see the benfitis (especially improved safety) immediatly. For example,
```
void alias(const int& c, int& m) {
    std::jthread t1([&m]() {
        m = 0;
    });
    std::jthread t2([&c]() {
        assert(c == 42);
    });
}
``` 
The obviously contrived function above will often assert in C++ if "c" and "m" alias. However in Swift, "c" is guaranteed to be a distinct, truely immutable reference (to uphold Swift's strict safety model). So calling this function in Swift will never assert:
```
var local: CInt = 42
test(local, &local)
```

### Computed properties

Value categories and mutability may require user input to map correctly in Swift, but constructs like iterators and getters and setters, can largely be imported automatically. Getters, setters, and subscripts can all be imported into Swift as computed properties. While many C++ codebases define a getter and setter pair, computed properties are the idiomatic way to handle this API pattern in Swift. And computed properties are not just about syntax, they also help promote safety and performance. For example, a C++ getter that returns a reference can be mapped into a generalized accessor in Swift that leverages coroutines to safely yield out its storage to the caller. This generalized accessor pattern allows safe and efficient access to C++ references in Swift and is another example of the more general philosophy for importing APIs: when Swift understands the semantics of an API it can map that API pattern to a strict Swift idiom that is safe, performant, and feels native, so that users get most of the benefits of Swift, even when calling C++ APIs. 

### Templates and generic APIs

C++ and Swift completely different models for generic programming. C++ templates provide textual specializations of functions and classes that are type checked after being specialized while Swift generics are type checked ahead of time, or separately, and based around APIs rather than textual substituion. The difference makes using generic C++ APIs in Swift difficult. Bridging C++ templates to the Swift model would require substantial engineering effort and the user's guidance. Even if this work was done and users were willing to annotate their C++ libraries sufficently, the Swift model may cause tention with C++ APIs that were designed with specific performance semantics in mind (for example, unboxing every element of a `vector` when performing a copy).

Swift protocols allow C++ types to be used in a generic context in Swift. Users can extend concrete C++ types, or concrete specializations of C++ class templates to conform to a protocol. Swift could provide a tool (likely in the form of an annotation) that allows users to even conform un-specialized templates to Swift protocols. This level of generic programming may be sufficent for most C++ interop users and would allow the Swift compiler to side-step one of the most difficult and complicated parts of C++ interoperability. 

[This forum post](https://forums.swift.org/t/bridging-c-templates-with-interop/55003) (Bridging C++ Templates with Interop) goes into depth on the issue of importing C++ templates into Swift.

## The standard library

Swift should provide an overlay for the C++ standard library to assist in the import of commonly used APIs (such as containers). This overlay should also provide helpful bridging utilites such as protocols that map imported ranges and iterators and explicit conversions from C++ types to standard Swift types.

C++ aims to provide sufficent tools to implement many features in The Standard Library rather than the compiler. While the Swift compiler also attempts to do this, it is not a goal in and of itself, resulting in many of C++'s analogus features being implemented in the compiler: tuples, pairs, reference counting, ownership, casting support, optionals, and so on. In these cases the Swift compiler will need to work with both the C++ standard library and the Swift overlay for the C++ standard library to import these APIs correctly.

The reverse is also true; C++ interop may require library-level Swift utilties to assist in the import of various C++ language concepts, such as iterators. To support this case, a set of C++ interop specific Swift APIs will be imported implicitly whenever a C++ module is imported. These APIs do not have a depedency on the distinct C++ standard library or its overlay. 

## Evolution

C++ interoperability is a huge feature that derives most of its benefit from the combination of its component features (for example, methods can't be used without types). C++ interop should be made useful to programmers before all component pieces have necessarily gone through evolution, both for the benefit of programmers wanting to use this feature, and for compiler developers designing and implementing the feature. 

C++ interoperability should bring in as many APIs as possible, even if they haven't gone through evolution. Swift evolution will progressively work through these APIs, formalizing them, and eventually interop will become a stable feature. Until a critical mass of APIs have been brought Swift's evolution process, a versioning scheme will allow C++ interoperability to be adopted and remain source stable while being evolved. Versions may be rapidly deprecated, but will be independent of Swift compiler versions, allowing source breaks even in minor compiler updates without disturbing adopters. 

This document allows specific, focused, and self contained evolution proposals to be created for individual pieces of the language and specific programming patterns by providing goals that lend themself to this kind of incremental design and evolution (by not importing everything and requiring specific mappings for specific API patterns) and by framing interop in a larger context that these individual evolution proposals can fit into.

## Tooling and build process

It goes without saying (yet will be said anyway) that as a supported language feature, C++ and Swift interoperability must work well on every platform supported by Swift. In a similar vein, tools in the Swift ecosystem should be updated to support interoperability features. For example, SourceKit should provide autocompletion, jump-to-definition, etc. for C++ functions, methods, and types and lldb should be able to print C++ types (even in Swift frames). Finally, the Swift package manager should be updated with the necessary features to support building C++ dependencies.

This document outlines a strategy for importing APIs that rely on semantic information from the user. In order to make this painless for users across a variety of projects, Swift will need to provide both inline annotation support for C++ APIs and side-file support for APIs that cannot be updated. For Objective-C, this side-file is an APINotes file. As part of Swift and C++ interoperability, APINotes will either need to be updated to support C++ APIs, or another kind side-file will need to be created.

## Appendix 1: Examples and Definitions

**Reference Types** have reference semantics and object identity. A reference type is a pointer (or “reference”) to some object which means there is a layer of indirection. When a reference type is copied, the pointer’s value is copied rather than the object’s storage. This means reference types can be used to represent non-copyable types in C++. For real-world examples of C++ reference types, consider LLVM's [`Instruction` class](https://llvm.org/doxygen/IR_2Instruction_8h_source.html) or Qt's [`QWidget` class](https://github.com/qt/qtbase/blob/dev/src/widgets/kernel/qwidget.h).

**Manually Managed Reference Types**

Here a programmer has written a very large `StatefulObject` which contains many fields:

```
struct StatefulObject {
  std::array<std::string, 32> names;
  std::array<std::string, 32> places;
  // ...
  
  StatefulObject(const StatefulObject&) = delete;
  StatefulObject() = delete;

  StatefulObject *create() { return new StatefulObject(); }
};
```


Because this object is so expensive to copy, the programmer decided to delete the copy constructor. The programmer also decided that this object should be allocated on the heap, so they decided to delete the default constructor, and provide a create method in its place. 

In Swift, this `StatefulObject` should be imported as a reference type, as it has reference semantics.

**API Incorrectly Using Reference Types**

Here someone has written an API that uses `StatefulObject` as a value type.

```
StatefulObject makeAppState();
```

This will invoke a copy of `StatefulObject` which violates the semantics that the API was written with. To be useable from Swift, this API needs to be updated to pass the object indirectly (by reference):

```
StatefulObject *makeAppState(); // OK
const StatefulObject *makeAppState(); // OK
StatefulObject &makeAppState(); // OK
const StatefulObject &makeAppState(); // OK
```

**Immortal Reference Types**

Instances of `StatefulObject` above are manually managed by the programmer, they create it with the create method and are responsible for destroying it once it is no longer needed. However, some reference types need to exist for the duration of the program, these references types are known as “immortal.” Examples of these immortal reference types might be pool allocators or app contexts. Let’s look at a `GameContext` object which allocates (and owns) various game elements:

```
struct GameContext {
  // ...
  
  GameContext(const GameContext&) = delete;

  Player *createPlayer();
  Scene  *createScene();
  Camera *createCamera();
};
```

Here the `GameContext` is meant to last for the entire game as a global allocator/state. Because the context will never be deallocated, it is known as an “immortal reference type” and the Swift compiler can make certain assumptions about it. 

**Automatically Managed Reference Types**

While the `GameContext` will live for the duration of the program, individual `GameObject` should be released once they’re done being used. One such object is Player:

```
struct GameObject {
  int referenceCount;
  
  GameObject(const GameObject&) = delete;
};

void gameObjectRetain(GameObject *obj);
void gameObjectRelease(GameObject *obj);

struct Player : GameObject {
  // ...
};
```

Here Player uses the `gameObjectRetain` and `gameObjectRelease` function to manually manage its reference count in C++. Once the `referenceCount` hits `0`, the Player will be destroyed. Manually managing the reference count is prone to errors, as programmers may forget to retain or release the object. Fortunately, this kind of reference counting is something that Swift is very good at. To enable automatic reference counting, the user can specify the retain and release operations via attributes directly on the `GameObject`. This means the programmer no longer needs to manually call `gameObjectRetain` and `gameObjectRelease`; Swift will do this for them. They will also benefit from the suite of ARC optimizations that Swift has built up over the years. 

**Owned types** “own” some storage which can be copied and destroyed. An owned type must be copyable and destructible. The copy constructor must copy any storage that is owned by the type and the destructor must destroy that storage. Copies and destroys must balance out and these operations must not have side effects. Examples of owned types include `std::vector` and `std::string`.

**Trivial types** are a subset of owned types. They can be copied by copying the bits of a value of the trivial type and do not need any special destruction logic. Examples of trivial types are `std::array` and `std::pair<int, int>`. 

**Pointer Types** are trivial types that hold pointers or references to some un-owned storage (storage that is not destroyed when the object is destroyed). Pointer types are *not* a subset of trivial types or owned types. Examples of pointer types include `std::string_view` and `std::span` and raw pointer types such as `int *` or `void *`.

**Projections** are values rather than types. An example of a method which yields a projection is the `c_str` method on `std::string`.

```
struct string { // String is an owned type.
  char *storage;
  size_t size;
  
  char *c_str() { return storage; } // Projects internal storage
```

Iterators are also projections:

```
  char *begin() { return storage; } // Projects internal storage
  char *end() { return storage + size; } // Projects internal storage
```

Because `string` is an owned type, the Swift compiler cannot represent a projection of its storage, so the `begin`, `end`, and `c_str` APIs are not imported. A projection is only valid as long as the storage it points to is valid. Projections of reference types are usually safe because reference types have storage with long, stable lifetimes, but projections of owned types are more dangerous because the storage associated with a specific copy usually has a much shorter lifetime (therefore most of these projections of owned storage cannot yet be imported).


## Appendix 2: Lifetime and safety of self-contained types and projections

The following section will go further into depth on the issues with using projections of self contained types in Swift, rather than proposing a solution on how to import them. Let’s start with an example Swift program that naively imports some self-contained type and returns a projections of it:

```
var v = vector(1)
let start = v.begin()
doSomething(start)
fixLifetime(v)
```

To understand the problem with this code, the following snippet highlights where an implicit copy is created and destroyed:

```
var v = vector(1)
let copy = copy(v)
let start = copy.begin()
destroy(copy)
doSomething(start)
fixLifetime(v)
```

Here, because Swift copies `v` into a temporary with a tight lifetime before the call to `begin`, `v` projects a dangling reference. This is an example of how subtly different lifetime models make using C++ types from Swift hard, if their semantics aren’t understood by the compiler.

To make these APIs safe and usable, Swift cannot import unsafe projections of types that own memory, because they don’t fit the Swift model. Instead, the Swift compiler can try to infer what, semantically, the API is trying to do, or the library author can provide this information via annotations. In this case, the Swift compiler can infer that begin returns an iterator, which Swift can represent through the existing, safe Swift iterator interface. In the example above, “start” is a pointer type. Using this pointer returned by the “begin” method is unsafe, but the type of start itself is not unsafe. In other words, safety restrictions need not be applied to pointer types themselves but rather their unsafe uses.

C++ often projects the storage of owned types. C++ is able to tie the lifetime of the projection to the source using lexcal scopes. Because there is a well-defined, lexical point in which objects are destroyed, C++ users can reason about projection’s lifetimes. While these safety properties are less formal than Swift, they are safety properties none-the-less, and form a model that works in C++.

This model cannot be adopted in Swift, however, because the the same lexical lifetime model does not exist. Further, projections of self-contained types are completely foreign concept in Swift, meaning users aren’t familiar with programming in terms of this lexical model, and may not be aware of the added (implicit) constraints (that is, when objects are destroyed). Swift’s language model is such that returning projections from a copied value, even in smaller lexical scope, should be safe. In order to allow projections of self-contained types, this assumption must be broken, or C++ interoperability must take advantage of Swift ownership features to associate the lifetime of the projection to the source.

The following example highlights the case described above:

```
func getCString(str: std.string) -> UnsafePointer<CChar> { str.c_str() }
```

The above function returns a dangling reference to `str`‘s inner storage. In C++, it is assumed that the programmer understands this is a bug, and generally would be expected to take `str` by reference. This is not the case in Swift. To represent this idiomatically in Swift, the lifetimes must be associated through a projection. Using the tools provided in the ownership manifesto this would mean yielding the value returned by `c_str` out of a [generalized accessor](https://github.com/apple/swift/blob/main/docs/OwnershipManifesto.md#generalized-accessors)(resulting in an error when the pointer is returned).
