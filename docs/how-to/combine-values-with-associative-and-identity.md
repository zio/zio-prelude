---
id: combine-values-with-associative-and-identity
title: "How to Combine Values with Associative and Identity"
description: "Merge response-time statistics from many servers with Associative and Identity instead of hand-written folds."
keywords:
  - "Combining Values"
  - "Aggregation"
  - "Associative"
  - "Identity"
  - "Monoid"
  - "foldMap"
---

In this guide, we aggregate response times reported by many servers into one set of statistics using `Associative` and `Identity`. By the end, you will have a `Stats` type that combines with the `<>` operator, folds over any collection, merges per-host maps, and handles empty input without special cases.

We start with the built-in instances for sums and maximums, then define our own, and finally combine whole collections and maps.

## The Problem

Aggregating values usually starts as a hand-written loop with a mutable accumulator. Each new statistic adds another variable, and the empty case needs its own handling:

```scala mdoc:compile-only
def summarize(responseTimes: List[Int]): (Int, Long, Int) = {
  var count   = 0
  var total   = 0L
  var largest = Int.MinValue

  responseTimes.foreach { time =>
    count += 1
    total += time
    largest = math.max(largest, time)
  }

  (count, total, largest)
}
```

This works for one list. It cannot merge the statistics of two servers without re-running over the raw data, it uses `var`, and the three accumulators are loosely tied together by a tuple. The rule for combining two summaries is implicit in the loop and cannot be reused.

In this guide, we write that rule once as an `Identity` instance, and get folding, merging and the empty case from it.

## Prerequisites

We need the `zio-prelude` dependency:

```scala
// Type classes, data types, newtypes and ZPure. JVM, Scala.js, Native.
libraryDependencies += "dev.zio" %% "zio-prelude" % "@VERSION@"
```

An `Associative[A]` says that two `A` values can be combined into one, and that the grouping does not matter: `(a <> b) <> c` equals `a <> (b <> c)`. An `Identity[A]` adds a neutral value that leaves any other value unchanged. We import the package and the `Sum` and `Max` newtypes used below:

```scala mdoc:silent:reset
import zio.prelude._
import zio.prelude.newtypes.{Max, Sum}
```

## Combining with Built-In Instances

Plain `Int` has no `Associative` instance, because addition and maximum are both valid ways to combine integers and the compiler cannot choose. A newtype states the choice. `Sum` adds, and `Max` keeps the larger value, and both combine with the `<>` operator:

```scala mdoc
Sum(120) <> Sum(80)

Max(120) <> Max(80)
```

Collections have instances too. A `List`, a `String` and an `Option` of a combinable type combine as well, and `Option` treats `None` as the neutral value:

```scala mdoc
List(1, 2) <> List(3)

"response" <> "time"

Option(Sum(1)) <> None <> Option(Sum(2))
```

## Defining an Identity for Your Own Type

For our own type, we describe the neutral value and the combining function once. `Identity.make` takes both. The neutral value is a `Stats` that represents no observations, and the combining function adds the counts and totals and keeps the larger maximum:

```scala mdoc:silent
final case class Stats(count: Int, total: Long, largest: Int)

object Stats {
  implicit val statsIdentity: Identity[Stats] =
    Identity.make(
      Stats(0, 0L, Int.MinValue),
      (left, right) =>
        Stats(
          left.count + right.count,
          left.total + right.total,
          left.largest max right.largest
        )
    )

  def single(responseTime: Int): Stats =
    Stats(1, responseTime.toLong, responseTime)
}
```

We keep the instance in the companion object so the compiler finds it without an import. Two `Stats` combine with the same `<>` operator, and `Identity.apply` gives access to the neutral value:

```scala mdoc
Stats.single(120) <> Stats.single(80)

Identity[Stats].identity
```

:::warning
The combining function must be associative, and the neutral value must leave other values unchanged on both sides. The compiler cannot check this. [How to Test Instances with Laws](./test-instances-with-laws.md) shows how to test it.
:::

## Folding a Collection

With an `Identity`, the `ForEach#foldMap` operator turns each element into a `Stats` and combines them all. There is no accumulator to write, and an empty list returns the neutral value instead of failing:

```scala mdoc
List(120, 80, 300).foldMap(Stats.single)

List.empty[Int].foldMap(Stats.single)
```

To fold a collection that already contains combinable values, call `ForEach#fold`. We call it through the `ForEach` instance, because the standard library already has a `fold` method on `List` that takes two arguments:

```scala mdoc
ForEach[List].fold(List(Sum(120), Sum(80), Sum(300)))
```

## Merging Maps by Key

Each server reports its own statistics, keyed by host name. A `Map` has an `Identity` whenever its values have an `Associative` instance: values under the same key are combined, and other keys are kept. So merging two reports is one `<>`:

```scala mdoc
val morning: Map[String, Stats] =
  Map("api" -> Stats.single(10), "db" -> Stats.single(20))

val evening: Map[String, Stats] =
  Map("api" -> Stats.single(30))

morning <> evening
```

Notice that the `api` entry combines both observations (`count` is `2`), while the `db` entry is carried over. Combining a whole list of reports works the same way with `ForEach#fold`.

## Combining Without a Neutral Value

Some types can be combined but have no neutral value. The time range covered by some observations is an example: there is no range that leaves every other range unchanged. For these we define only an `Associative`, with `Associative.make`:

```scala mdoc:silent
final case class Bounds(lowest: Int, highest: Int)

object Bounds {
  implicit val boundsAssociative: Associative[Bounds] =
    Associative.make((left, right) =>
      Bounds(left.lowest min right.lowest, left.highest max right.highest)
    )
}
```

Folding needs a starting value, so `ForEach#reduceAssociative` returns an `Option`. It is `None` for an empty collection:

```scala mdoc
List(Bounds(3, 5), Bounds(1, 2)).reduceAssociative

List.empty[Bounds].reduceAssociative
```

## Going Further

- If the order of arguments never matters, define a `Commutative` with `Commutative.make`. See [Commutative](../functional-abstractions/concrete-types/commutative.md).
- For types where each value can be undone, see [Inverse](../functional-abstractions/concrete-types/inverse.md).
- Read the reference pages for [Associative](../functional-abstractions/concrete-types/associative.md) and [Identity](../functional-abstractions/concrete-types/identity.md), and [How to Traverse Collections with ForEach](./traverse-with-foreach.md) for more operators that use them.
