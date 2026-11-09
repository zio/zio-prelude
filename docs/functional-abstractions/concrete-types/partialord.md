---
id: partialord
title: "PartialOrd"
---

`PartialOrd[A]` describes an ordering on values of type `A` in which some pairs of values may not be comparable at all. It is a weaker form of `Ord`, which requires every pair of values to be ordered.

Set inclusion is a common example. One set can contain another, or the two can be equal, but two sets can also overlap without either containing the other. Those two sets are not greater or less than each other, and `Ord` cannot express that. `PartialOrd` can.

`compare` returns a `PartialOrdering`. The result is one of three orderings, `Ordering.LessThan`, `Ordering.Equals`, and `Ordering.GreaterThan`, or it is `PartialOrdering.Incomparable` when the two values cannot be compared.

## Defining a PartialOrd

We create an instance with `PartialOrd.make`, which takes a function that returns a `PartialOrdering`. The following instance orders sets by inclusion:

```scala mdoc:silent:reset
import zio.prelude._

implicit val setInclusion: PartialOrd[Set[Int]] =
  PartialOrd.make { (left, right) =>
    if (left == right) Ordering.Equals
    else if (left.subsetOf(right)) Ordering.LessThan
    else if (right.subsetOf(left)) Ordering.GreaterThan
    else PartialOrdering.Incomparable
  }
```

Comparing a set with a set it is contained in gives `LessThan`, and comparing two sets that overlap without either containing the other gives `Incomparable`:

```scala mdoc
PartialOrd[Set[Int]].compare(Set(1), Set(1, 2))

PartialOrd[Set[Int]].compare(Set(1, 2), Set(2, 3))
```

## Building Instances From Other Instances

`PartialOrd` provides combinators that build an instance for a pair of values from instances for each component. `both` compares the first components of two pairs, and only if they are equal does it compare the second components. `bothWith` does the same after converting a value with a function, which lets us describe an ordering for a case class in terms of its fields.
