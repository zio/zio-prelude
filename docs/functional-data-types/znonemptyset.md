---
id: znonemptyset
title: "ZNonEmptySet"
---

`ZNonEmptySet[A, B]` is a non-empty version of [ZSet](./zset.md). It holds at least one element `A`, and each element carries a weight `B` that says how much of it the set contains. The weight can be a count, a probability, or a boolean that says whether the element is present at all.

The most common case is a multiset, which is a set that counts how many times each element occurs. ZIO Prelude gives this case the name `NonEmptyMultiSet[A]`, which is a `ZNonEmptySet[A, Natural]`.

We import the type and the companion object from the `zio.prelude` package:

```scala mdoc:silent:reset
import zio.prelude._
```

## Constructing a Multiset

`ZNonEmptySet.apply` takes one element and any number of further elements. Each occurrence of an element adds one to its count:

```scala mdoc
val letters: NonEmptyMultiSet[String] =
  ZNonEmptySet("a", "b", "a")
```

`ZNonEmptySet.fromIterableOption` builds the set from an `Iterable` and returns `None` when the collection is empty:

```scala mdoc
ZNonEmptySet.fromIterableOption(List("x", "x", "y")).map(_.toMap)

ZNonEmptySet.fromIterableOption(List.empty[String])
```

## Reading the Counts

`ZNonEmptySet#apply` returns the weight of an element, so `letters("a")` is the number of times `"a"` occurs. For an element that is not in the set, the weight is zero:

```scala mdoc
letters("a")

letters("z")
```

`toMap` returns the weight of every element as a plain `Map`:

```scala mdoc
letters.toMap
```

`toNonEmptySet` discards the weights and returns a [NonEmptySet](./nonemptyset.md) of the elements. The result is still non-empty, because an element that occurs at least once is still present:

```scala mdoc
letters.toNonEmptySet.toSet
```

## Combining and Transforming

`combine` adds the weights of two sets, so an element's weight becomes the sum of its weights in both sets. `union` takes the maximum weight instead.

`map` transforms each element. If two elements become equal, their weights are added together, so no occurrences are lost:

```scala mdoc
letters.map(_.toUpperCase).toMap
```

`flatMap` replaces each element with a new set, and each weight is multiplied by the weight of the new set's element. `flatMap` is the operator to reach for when each element expands into several elements.
