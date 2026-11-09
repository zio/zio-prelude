---
id: index
title: "Functional Data Types in ZIO Prelude"
sidebar_label: "Functional Data Types"
---

ZIO Prelude includes several data types to help us model our domains more accurately and solve common problems.

- **[Equivalence](equivalence.md)** - A description of an equivalence relationship between two data types.
- **[NonEmptyList](nonemptylist.md)** - A list that is guaranteed to be non-empty to more accurately model situations where we know a collection has at least one element.
- **[Id and Const](id-and-const.md)** - Two newtypes that give effect-free meaning to a type constructor's parameters, used with operators such as `ForEach`.
- **[NonEmptyMap](nonemptymap.md)** - A map that is guaranteed to contain at least one entry.
- **[NonEmptySet](nonemptyset.md)** - A set that is guaranteed to contain at least one element.
- **[NonEmptySortedMap and NonEmptySortedSet](nonemptysorted.md)** - Sorted versions of `NonEmptyMap` and `NonEmptySet`, keeping their keys or elements in `Ordering` order.
- **[Optional](optional.md)** - A value that may be present or absent, with instances for the functional abstractions.
- **[ParSeq](parseq.md)** - A description of events that happen in sequence or in parallel, useful for modeling several failures or several requests.
- **[Recursive](recursive.md)** - A way to describe recursive data structures and fold them without writing the recursion by hand.
- **[These](these.md)** - A data type that may either be a `Left` with an `A`, a `Right` with a `B`, or a `Both` with an `A` and a `B`, useful for modeling problems such as merging streams of data.
- **[Validation](validation.md)** - A data type that may be either a success or an accumulation of one or more errors, allowing modeling multiple failures for applications such as data validation.
- **[ZNonEmptySet](znonemptyset.md)** - A non-empty weighted set, such as a multiset that is guaranteed to contain at least one element.
- **[ZSet](zset.md)** - A generalization of a set that generalizes measures of "how many" of an element exist in a set, supporting multi-sets, "fuzzy" sets, and other data structures.
- **[ZValidation](zvalidation.md)** - A generalization of `Validation` that allows maintaining a log of warnings in addition to accumulating errors.
