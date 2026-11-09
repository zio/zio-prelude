---
id: partialinverse
title: "PartialInverse"
---

`PartialInverse[A]` describes an associative operator on `A` that has an identity element and an inverse operator. It is like [Inverse](./inverse.md), but the inverse does not have to exist for every pair of values.

The natural numbers are a case where a total inverse does not exist. Addition has an inverse, subtraction, but subtraction can give a negative number, and the natural numbers do not include negative numbers. So the inverse is only defined when the result stays natural.

`PartialInverse` extends `Identity`, so it provides `combine` and an `identity` element. Its own operation is `inverseOption`, which returns `Some` with the inverse when it is defined and `None` otherwise. Combining a value with itself using the inverse always gives the identity element.

## Defining a PartialInverse

We create an instance with `PartialInverse.make`, which takes the identity element, the operator, and the inverse. For natural numbers, the operator is addition, the identity is zero, and the inverse is subtraction that returns `None` when the result would be negative:

```scala mdoc:silent:reset
import zio.prelude._

val naturals: PartialInverse[Int] =
  PartialInverse.make[Int](
    0,
    _ + _,
    (l, r) => if (l >= r) Some(l - r) else None
  )
```

Subtracting a smaller number from a larger one succeeds, and the other way round returns `None`:

```scala mdoc
naturals.inverseOption(5, 2)

naturals.inverseOption(2, 5)
```

Combining a value with itself using the inverse gives the identity element, which is zero:

```scala mdoc
naturals.inverseOption(7, 7)
```

`PartialInverse.makeFrom` builds the same kind of instance from an existing `Identity` instance and an inverse function, when the identity and the operator are already defined elsewhere.
