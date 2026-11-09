---
id: constructing
title: "Constructing and Unwrapping New Types"
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

A new type has a companion object that provides the operators for creating its values and for converting them back to the underlying type. This guide covers those operators. For the basic definition of a new type, see [New Types](./index.md).

Throughout this guide we use a `SequenceNumber` new type over `Int`:

```scala mdoc:silent:reset
import zio.Chunk
import zio.prelude.Newtype

object SequenceNumber extends Newtype[Int]
type SequenceNumber = SequenceNumber.Type
```

## Constructing a Single Value

The easiest way to construct an instance of a new type is to call `apply` on the new type object:

```scala mdoc
val sequenceNumber: SequenceNumber =
  SequenceNumber(1)
```

You can think of the new type object as the companion object of the new type. It comes with `apply` so that we can construct values of the new type without any extra ceremony.

When we pass several values to `apply`, we get a `NonEmptyChunk` of the new type back. This is convenient when we need a few values at once:

```scala mdoc
val sequenceNumbers: zio.NonEmptyChunk[SequenceNumber] =
  SequenceNumber(1, 2, 3)
```

Scala 2 supports this with a single varargs overload, and Scala 3 supports it with overloads for up to six values.

## Constructing a Collection

If we want to construct a collection of new type values from a collection of underlying values, we can use `wrapAll`:

```scala mdoc
val wrapped: Chunk[SequenceNumber] =
  SequenceNumber.wrapAll(Chunk(3, 4, 5))
```

`wrapAll` does not traverse the collection. Internally the new type and its underlying type have the same runtime representation, so the `Chunk[Int]` is reinterpreted as a `Chunk[SequenceNumber]` without any work.

## Wrapping a Value Directly

The `wrap` operator converts a single underlying value to the new type without any other checks. Whether `wrap` is accessible outside the new type object depends on the Scala version. In Scala 2 it is `protected`, so only code inside the object can call it. In Scala 3 it is public.

<Tabs groupId="scala-version" defaultValue="scala2">
  <TabItem value="scala2" label="Scala 2">

```scala mdoc:compile-only
import zio.prelude.Newtype

object Meter extends Newtype[Double] {
  val oneAndAHalf: Meter =
    wrap(1.5)
}
type Meter = Meter.Type
```

  </TabItem>
  <TabItem value="scala3" label="Scala 3">

```scala
import zio.prelude.Newtype

object Meter extends Newtype[Double]
type Meter = Meter.Type

val oneAndAHalf: Meter =
  Meter.wrap(1.5)
```

  </TabItem>
</Tabs>

## Extending the Constructors

The new type object is also the natural place for our own constructors. To define an `initial` sequence number in terms of `apply`:

```scala mdoc:silent:reset
import zio.Chunk
import zio.prelude.Newtype

object SequenceNumber extends Newtype[Int] {
  def initial: SequenceNumber =
    SequenceNumber(0)
}
type SequenceNumber = SequenceNumber.Type
```

Now the constructor is available on the new type just as it would be on the companion object of any ordinary data type, so `SequenceNumber.initial` can be used anywhere a sequence number is expected. The unwrapping example below shows its underlying value.

## Unwrapping a Value

To access the underlying value and work with it, we use `unwrap` on the new type object:

```scala mdoc
val zero: Int =
  SequenceNumber.unwrap(SequenceNumber.initial)
```

To convert a collection of new type values back to underlying values, we use `unwrapAll`:

```scala mdoc
val ints: Chunk[Int] =
  SequenceNumber.unwrapAll(SequenceNumber.wrapAll(Chunk(1, 2, 3)))
```

Like `wrapAll`, `unwrapAll` does not traverse the collection, because the underlying values already have the representation the caller needs.
