---
id: partialequivalence
title: "PartialEquivalence"
---

`PartialEquivalence[A, B, E1, E2]` describes a conversion between two types `A` and `B` in both directions, where either direction can fail. It generalizes [Equivalence](../../functional-data-types/equivalence.md), which requires both conversions to always succeed.

For example, a string such as `"42"` can be parsed into an `Int`, but a string such as `"forty-two"` cannot. Parsing can fail, and converting an `Int` back into a string always succeeds. A `PartialEquivalence` describes that situation directly.

The type has two functions. `toPartial` converts an `A` into an `Either[E1, B]`, and `fromPartial` converts a `B` into an `Either[E2, A]`.

## Defining a PartialEquivalence

We create an instance with `PartialEquivalence.apply`, which takes the two conversions. Here the conversion from a string to an `Int` reports an error message when the string is not a number:

```scala mdoc:silent:reset
import zio.prelude._

val intFromString: PartialEquivalence[String, Int, String, String] =
  PartialEquivalence(
    (s: String) => s.toIntOption.toRight(s"not a number: $s"),
    (i: Int) => Right(i.toString)
  )
```

Converting a valid string succeeds, and converting an invalid one returns the error:

```scala mdoc
intFromString.toPartial("42")

intFromString.toPartial("forty-two")
```

Converting back from an `Int` to a string always succeeds:

```scala mdoc
intFromString.fromPartial(42)
```

## Reversing and Chaining

`flip` swaps the two directions, so the result converts from `B` to `A`. The operator `>>>` chains two partial equivalences, so the output of the first becomes the input of the second. A chain fails as soon as one of its steps fails:

```scala mdoc:silent
val stringFromInt: PartialEquivalence[Int, String, String, String] =
  intFromString.flip

val doubled: PartialEquivalence[Int, Int, String, Nothing] =
  PartialEquivalence(
    (i: Int) => Right(i * 2),
    (i: Int) => Right(i / 2)
  )

```

```scala mdoc
(intFromString >>> doubled).toPartial("21")

