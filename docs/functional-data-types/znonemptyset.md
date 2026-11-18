---
id: znonemptyset
title: "ZNonEmptySet"
---

`ZNonEmptySet[A, B]` is a non-empty version of [ZSet](./zset.md). It holds at least one element `A`, and each element carries a weight `B` that says how much of it the set contains. The weight can be a count, a probability, or a boolean that says whether the element is present at all.

The most common case is a multiset, which is a set that counts how many times each element occurs. ZIO Prelude gives this case the name `NonEmptyMultiSet[A]`, which is a `ZNonEmptySet[A, Natural]`.

## Motivation

A plain `Set` forgets how many times an element was seen, and a `Map[A, Int]` allows zero counts and an empty map, so every consumer must decide what those mean. `ZNonEmptySet` removes both problems. It records the weight of every element with a type that can be summed and multiplied, and it guarantees that at least one element is present, so reading the "most common word" or the "first item" never needs an empty-case branch. Typical uses are word frequencies, a basket of items with quantities, and a collection of weighted outcomes.

Use [ZSet](./zset.md) when the collection may be empty, [NonEmptySet](./nonemptyset.md) when each element is either present or not and has no weight, and `ZNonEmptySet` when you need both weights and the non-empty guarantee.

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

`toNonEmptySet` discards the weights, dropping any element, other than the first, whose weight equals the zero of the weight type, and returns a [NonEmptySet](./nonemptyset.md) of the elements. The result is always non-empty, because the first element is always kept:

```scala mdoc
letters.toNonEmptySet.toSet
```

The other constructors cover weights you already have. `ZNonEmptySet.fromNonEmptyChunk` counts the elements of a `NonEmptyChunk`, `ZNonEmptySet.fromNonEmptySet` produces a set with `Boolean` weights, and `ZNonEmptySet.fromMapOption` and `ZNonEmptySet.fromSetOption` return `None` for an empty input. `ZNonEmptySet#toZSet` forgets the non-empty guarantee, and `ZNonEmptySet#transform` changes the weights while keeping the elements.

## Combining and Transforming

`ZNonEmptySet#combine`, also written `<>`, adds the weights of two sets, so an element's weight becomes the sum of its weights in both sets. `ZNonEmptySet#union`, also written `|`, takes the maximum weight instead. The other argument may be any `ZSet`, so a non-empty set can absorb a possibly empty one and stay non-empty. The following example merges the stock counted in the morning with a delivery:

```scala mdoc
val morning: NonEmptyMultiSet[String] = ZNonEmptySet("apple", "pear", "apple")

val delivery: NonEmptyMultiSet[String] = ZNonEmptySet("pear", "fig")

morning.combine(delivery).toMap

morning.union(delivery).toMap
```

`map` transforms each element. If two elements become equal, their weights are added together, so no occurrences are lost:

```scala mdoc
letters.map(_.toUpperCase).toMap
```

`flatMap` replaces each element with a new set, and each weight is multiplied by the weight of the new set's element. `flatMap` is the operator to reach for when each element expands into several elements:

```scala mdoc
letters.flatMap(letter => ZNonEmptySet(letter, letter.toUpperCase)).toMap
```

`ZNonEmptySet#zip` pairs every element of one set with every element of another, and multiplies the weights of each pair.

## Instances and Laws

`ZNonEmptySet` has `Equal`, `Hash` and `Debug` instances when the element and weight types provide them, a `Covariant` and an `AssociativeFlatten` instance over the element type, and `Associative`, `Commutative` and `Idempotent` instances that combine two sets by combining the weights.

## Related Abstractions

[ZSet](./zset.md) is the version that may be empty, [NonEmptySet](./nonemptyset.md) is the version without weights, and [NonEmptyMap](./nonemptymap.md) keeps a value of any type per key. `ParSeq#toNonEmptyMultiSet` counts the events of a parallel-sequential collection.
