---
id: subtypes
title: "Subtypes"
---

When we use `Newtype`, the new type is completely distinct from its underlying type. This is useful when we want to hide the underlying representation and unwrap only when we say so.

The cost is boilerplate. If we want to use an operator that already exists on the underlying type, we have to unwrap the value or reimplement the operator for the new type.

For example, say we want to test whether one `SequenceNumber` comes after another. With a plain `Newtype` this does not compile:

```scala mdoc:silent:reset
import zio.prelude.Newtype

object SequenceNumber extends Newtype[Int]
type SequenceNumber = SequenceNumber.Type
```

Attempting the comparison now fails to compile:

```scala mdoc:fail
SequenceNumber(2) > SequenceNumber(1)
```

The `>` operator exists on `Int`, and the compiler sees `SequenceNumber` and `Int` as unrelated types. We could unwrap each value or define our own `>` on sequence numbers, but in both cases we are not using the fact that every `SequenceNumber` is an `Int`.

We keep that fact by extending `Subtype` instead of `Newtype`:

```scala mdoc:silent:reset
import zio.prelude.Subtype

object SequenceNumber extends Subtype[Int]
type SequenceNumber = SequenceNumber.Type
```

Now the comparison compiles and returns the result of `Int` comparison:

```scala mdoc
SequenceNumber(2) > SequenceNumber(1)
```

`SequenceNumber` is still a different type from `Int`, but it is a subtype of `Int`. Because of that, a `SequenceNumber` works anywhere an `Int` is required, and every operator on `Int` works on `SequenceNumber` directly:

```scala mdoc
val asInt: Int =
  SequenceNumber(3)

asInt + 1
```

We keep the type safety of the new type. An `Int` or an `AccountNumber` still cannot stand in for a `SequenceNumber`.

Use `Subtype` when we want to keep the underlying operators. Use `Newtype` when those operators would leak meaning we do not want, such as arithmetic on an account number.
