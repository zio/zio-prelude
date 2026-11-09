---
id: nonemptymap
title: "NonEmptyMap"
---

`NonEmptyMap` is a `Map` that is guaranteed to contain at least one entry. It works the same way as [NonEmptyList](./nonemptylist.md): the type records a fact we already know, so the code does not need to handle an empty map that cannot occur.

Consider a configuration map that must always contain a default entry. With a plain `Map`, every lookup of that default has to handle the possibility that the map is empty. With a `NonEmptyMap`, the type says the map has at least one entry, so the code can work with it directly.

We import `NonEmptyMap` from the `zio.prelude` package:

```scala mdoc:silent:reset
import zio.prelude.NonEmptyMap
```

## Constructing a NonEmptyMap

The easiest way to construct a `NonEmptyMap` is with `apply`, which takes the first entry and any number of further entries:

```scala mdoc
val prices: NonEmptyMap[String, Int] =
  NonEmptyMap("apple" -> 3, "pear" -> 5, "plum" -> 4)

prices.toMap
```

When we already have a `Map`, `NonEmptyMap.fromMapOption` returns `Some` if the map has an entry and `None` otherwise. The result is an `Option`, because the original map may be empty:

```scala mdoc
NonEmptyMap.fromMapOption(Map("apple" -> 3))

NonEmptyMap.fromMapOption(Map.empty[String, Int])
```

`NonEmptyMap.single` builds a map with one entry, which is useful as a starting point for further updates.

## Reading a NonEmptyMap

Because the map is never empty, `keySet` and `values` always return a non-empty collection. `keySet` returns a `NonEmptySet` of keys, and `values` returns a `NonEmptyChunk` of values:

```scala mdoc
prices.keySet

prices.values
```

`peel` splits the map into one entry and the rest of the map. The rest is a plain `Map`, which may be empty. `tailNonEmpty` returns the rest as an `Option[NonEmptyMap]`, so the empty case is handled explicitly:

```scala mdoc
prices.peel

prices.tailNonEmpty
```

## Updating a NonEmptyMap

`+` adds an entry, and `++` adds several entries at once. If the key is already present, the new value replaces the old one. Both return a `NonEmptyMap`, because adding entries cannot make the map empty:

```scala mdoc
(prices + ("fig" -> 6)).toMap
```

Removing an entry can make the map empty, so `remove` returns a plain `Map` rather than a `NonEmptyMap`:

```scala mdoc
prices.remove("apple")
```

`mapValues` transforms every value and keeps the keys, so the result is still a `NonEmptyMap`:

```scala mdoc
prices.mapValues(_ * 2).toMap
```

## Grouping Values into a NonEmptyMap

`NonEmptyMap.groupByOption` groups the elements of a collection by a key function. It returns `None` when the collection is empty, and otherwise a `NonEmptyMap` whose values are the groups:

```scala mdoc
NonEmptyMap.groupByOption(List("apple", "avocado", "pear"))(_.head)
  .map(_.mapValues(_.toList).toMap)
```
