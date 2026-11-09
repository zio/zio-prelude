---
id: nonemptysorted
title: "NonEmptySortedMap and NonEmptySortedSet"
---

`NonEmptySortedMap` and `NonEmptySortedSet` are the sorted versions of [NonEmptyMap](./nonemptymap.md) and [NonEmptySet](./nonemptyset.md). Each guarantees at least one entry or element, and each keeps its entries in the order given by an `Ordering` from the Scala standard library.

Use these types when the non-empty guarantee and a stable sorted order are both part of what the code relies on. For example, the lowest priority in a queue is always the first element of a sorted set, and that element always exists.

We import both types from the `zio.prelude` package:

```scala mdoc:silent:reset
import zio.prelude.{NonEmptySortedMap, NonEmptySortedSet}
```

## NonEmptySortedSet

The `NonEmptySortedSet` constructors need an implicit `Ordering` for the element type. For `String`, the standard library provides one:

```scala mdoc
val tags: NonEmptySortedSet[String] =
  NonEmptySortedSet("scala", "zio", "docs")

tags.toSet
```

The elements come out in sorted order, and duplicates are removed. `NonEmptySortedSet.fromSetOption` converts an existing `SortedSet` and returns `None` when it is empty:

```scala mdoc
NonEmptySortedSet.fromSetOption(scala.collection.immutable.SortedSet.empty[String])
```

`head` returns the first element in the sorted order, which is always present:

```scala mdoc
tags.head
```

`removeNonEmpty` removes an element and returns `Some` with the remaining set if there is anything left, otherwise `None`:

```scala mdoc
tags.removeNonEmpty("zio").map(_.toSet)
```

## NonEmptySortedMap

`NonEmptySortedMap` works like `NonEmptyMap`, but its keys are kept in sorted order and its constructors need an implicit `Ordering` for the key type.

```scala mdoc
val scores: NonEmptySortedMap[String, Int] =
  NonEmptySortedMap("carol" -> 7, "alice" -> 9, "bob" -> 4)

scores.toMap
```

`keySet` returns the keys as a `NonEmptySortedSet`, so the sorted order carries over to the keys:

```scala mdoc
scores.keySet.toSet
```

`peel` splits the map into its first entry and the rest. The first entry is the one with the smallest key:

```scala mdoc
scores.peel
```

Updates behave as they do for `NonEmptyMap`. `+` and `++` return a `NonEmptySortedMap`, while `remove` returns a plain `SortedMap`, because removing an entry can make the map empty.
