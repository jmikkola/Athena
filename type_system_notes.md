# Type Systlem Notes

There are three mappings to consider in the type system.

1. Mapping type names to their definition
   - What's a definition in the case of something like a primative?
2. Mapping subtypes to supertypes
3. Resolving type alises

## Type Aliases

Let's consider these in reverse order. Type aliases aren't actually
currently supported in the language, but it's good to think ahead to
make sure they aren't hard to support later.

For example, the type alias `MyStrings`, defined as

    type MyStrings List<String>

can be used interchangebly with the type `List<String>`. This
resolution can be done before starting to perform typechecking, which
removes it as a concern from that process (that is, every use of the
type `MyStrings` can simply be replaced with `List<String>`). Type
aliases are not subtypes, so this is safe.

It's worth noting that the process of replacing type alises is
recursive. For example, with the set of types:

    type MyStrings List<String>
    type MyMap Map<String, MyStrings>
    type OtherMap MyMap

requires repeated substitution when starting from `OtherMap` to find
the final type (`Map<String, List<String>>`).

## Supertypes

At present, the language does not support interfaces (though it will
soon) so the only way for one type to be a subtype of another is for
it to be a record inside an Enum. When interfaces are added, it will
become possible for a type to be a suptype of multiple other types (by
implementing multiple interfaces). It will also become possible to
explicitly create a subtype (for the sake of added type safety).

Supertype relationships can be stored as a mapping from subtype to set
of supertypes. It can also be represented as the function:

    superTypes :: Type -> Set Type

where the set may be empty if the type is unknown.

The process of determining the set of supertypes is recursive. Consider:

    type A subtype(Int)
    type B subtype(A)

The supertypes of `B` are `A`, `Int`, and the interfaces that `Int`
implements (such as `Eq`, `Show`, etc).

A type T1 is allowed to be used where T2 is required if T2 equals T1
or if T2 appears in the set of supertypes of T1.

Open question: How to handle this with generics?

## Types to definitions

This is probably the most tricky part.

Consider:

    type A struct{ i Int }
    type B struct{ i Int }

Both structures are physically identical, yet they should not be
considered the same type. That is, a function expecting a value of
type `A` should not accept one of type `B`.

For these purposes, it would seem to make sense to just compare types
by name. However, that is not always possible. Consider:

    f :: () List<String>
    g :: (List<String>) Int

The expression `g(f())` should typecheck, even though the type
`List<String>` is never given a name.

Recalling the fact that type aliases won't be present at this stage
and the fact that the only other way to have multiple ways to refer to
a type is through subtypes, the solution becomes a bit easier.

We can compare names to names and explicit structs to explicit
structures, and only consider names and explicit structures together
when one is the subtype of another. That is to say, just compare them
directly with the subtype comparison process and ignore resolving type
names.
