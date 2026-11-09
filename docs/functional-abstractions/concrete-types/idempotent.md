---
id: idempotent
title: "Idempotent"
---

`Idempotent[A]` describes an associative operator on `A` where combining a value with an identical value gives that same value back. In symbols, `a <> a` equals `a` for every value `a`.

Set union is idempotent, because a set combined with itself is still the same set. Integer addition is not, because adding a number to itself doubles it.

Idempotent operators are useful for optimization. When an operator is idempotent, combining the same value many times gives the same result as combining it once, so duplicate work can be skipped.

`Idempotent` extends `Associative`, so every idempotent instance also has an associative `combine`.

## Using an Existing Instance

The library provides idempotent instances for some types. `Max[Int]` is idempotent, because the maximum of a value and itself is that value:

```scala mdoc:silent:reset
import zio.prelude._
import zio.prelude.newtypes._
```

Next, we combine two values with the operator:

```scala mdoc
Idempotent[Max[Int]].combine(Max(3), Max(3))
```

Boolean disjunction is idempotent as well, because `true` or `true` is `true`, and `false` or `false` is `false`:

```scala mdoc
Idempotent[Or].combine(Or(true), Or(true))
```

## Skipping Work for Equal Values

`combineIdempotent` is a method on every `Idempotent` instance. It checks whether the two values are equal, and if they are, returns the first one without calling `combine`. Otherwise it combines them normally:

```scala mdoc
Idempotent[Max[Int]].combineIdempotent(Max(3), Max(3))
```

The `idempotent` method on an `Idempotent` instance returns a new instance whose `combine` uses `combineIdempotent`, so the equality check runs on every combination. This is useful when `combine` is expensive and many equal values are combined.

The shortcut is only valid when the operator is idempotent. On an operator that is not idempotent, `combineIdempotent` returns a different result from `combine` whenever the two values are equal.
