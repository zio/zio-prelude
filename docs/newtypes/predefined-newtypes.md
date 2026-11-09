---
id: predefined-newtypes
title: "Predefined Newtypes"
---

ZIO Prelude ships with a set of new types in the `zio.prelude.newtypes` package. Each one wraps a common type and gives it a meaning that matters for a particular operation, such as summing values or keeping the first one. Because these new types come with their instances, we can use them to combine values without writing any instances ourselves.

To use them, we import the package:

```scala mdoc:silent:reset
import zio.prelude._
import zio.prelude.newtypes._
```

## Sum and Product

`Sum` represents addition and `Prod` represents multiplication. Both are parameterized by the numeric type they wrap. Combining two sums adds the values, and combining two products multiplies them.

To combine two `Sum` values, we use the `Associative` instance for `Sum[Int]`:

```scala mdoc
Associative[Sum[Int]].combine(Sum(1), Sum(2))
```

The same operation on `Prod` multiplies the values:

```scala mdoc
Associative[Prod[Int]].combine(Prod(3), Prod(4))
```

## Minimum and Maximum

`Min` represents taking the smaller of two values and `Max` represents taking the larger. Both are parameterized by the type they compare.

To take the maximum of two values:

```scala mdoc
Associative[Max[Int]].combine(Max(3), Max(5))
```

## First and Last

`First` keeps the first of two values and `Last` keeps the second. They are useful when we want to pick a value from a sequence of combined results.

To keep the first of two values:

```scala mdoc
Associative[First[Int]].combine(First(1), First(2))
```

To keep the last of two values:

```scala mdoc
Associative[Last[Int]].combine(Last(1), Last(2))
```

## Logical And and Or

`And` and `Or` represent logical conjunction and disjunction over `Boolean`. They are subtypes of `Boolean`, so the operators that `Boolean` already has, such as `&&` and `||`, are still available on them. They give the compiler a way to pick the right instances, because a `Boolean` could mean either operation.

To combine two `Or` values with logical disjunction:

```scala mdoc
Associative[Or].combine(Or(true), Or(false))
```

To combine two `And` values with logical conjunction:

```scala mdoc
Associative[And].combine(And(true), And(false))
```

## Natural Numbers

`Natural` is a smart newtype over `Int` whose values must be zero or more. Its assertion is checked the same way as in [Smart Newtypes](./smart-newtypes.md): a literal is checked at compile time, and a run-time value is checked with `Natural.make`.

To construct a natural number from a literal:

```scala mdoc
val zero: Natural = Natural(0)
```

To validate a run-time value, which here is negative and therefore rejected:

```scala mdoc
val negative = -1

Natural.make(negative)
```

## Failure Newtypes

`Failure`, `FailureIn`, and `FailureOut` are marker new types for values that describe a failure. The source gives each one a short description, and each one marks whether an error belongs to a failed state, an input, or an output:

| Type         | Meaning in the source                              |
| ------------ | -------------------------------------------------- |
| `Failure`    | Another type in a failed state                     |
| `FailureIn`  | An input error in another type                     |
| `FailureOut` | An output error in another type                    |

The source gives only these one-line descriptions, so this guide does not show examples for them.
