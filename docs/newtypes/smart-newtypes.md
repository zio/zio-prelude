---
id: smart-newtypes
title: "Smart Newtypes"
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

The new types in the earlier guides accept any value of their underlying type. A `SequenceNumber` built on `Int` can be `42` or `-1`. In many cases that is fine: we just want to say that a value is a sequence number and not an account number.

In other cases we want to restrict the values the underlying type can take. For instance, a sequence number should never be negative. A smart newtype does this by defining an `assertion` that describes which underlying values are valid.

## Defining an Assertion

We create a smart newtype by extending `Newtype` or `Subtype` and defining an `assertion` method on the new type object. The syntax differs between Scala 2 and Scala 3 because the macro API changed, so the definition is shown in tabs. Both tabs use the `greaterThanOrEqualTo` assertion, which requires the value to be zero or more:

<Tabs groupId="scala-version" defaultValue="scala2">
  <TabItem value="scala2" label="Scala 2">

```scala mdoc:compile-only
import zio.prelude.{Subtype, Validation}
import zio.prelude.Assertion._

object SequenceNumber extends Subtype[Int] {
  override def assertion = assert {
    greaterThanOrEqualTo(0)
  }
}
type SequenceNumber = SequenceNumber.Type
```

  </TabItem>
  <TabItem value="scala3" label="Scala 3">

```scala
import zio.prelude.{Subtype, Validation}
import zio.prelude.Assertion._

object SequenceNumber extends Subtype[Int] {
  override inline def assertion: Assertion[Int] =
    greaterThanOrEqualTo(0)
}
type SequenceNumber = SequenceNumber.Type
```

  </TabItem>
</Tabs>

The examples below use the Scala 2 definition, which the docs build compiles. We define the same object again here so that the following blocks can use it:

```scala mdoc:silent:reset
import zio.prelude.{Subtype, Validation}
import zio.prelude.Assertion._

object SequenceNumber extends Subtype[Int] {
  override def assertion = assert {
    greaterThanOrEqualTo(0)
  }
}
type SequenceNumber = SequenceNumber.Type
```

## Validating Literals at Compile Time

When we construct a value with `apply` from a literal, the assertion is checked at compile time:

```scala mdoc
val valid: SequenceNumber = SequenceNumber(42)
```

If the literal does not satisfy the assertion, the program does not compile and the error explains which condition failed:

```scala mdoc:fail
val oops: SequenceNumber = SequenceNumber(-10)
```

Compile-time validation only works for literals. In Scala 2, passing a variable or a run-time value to `apply` is a compilation error as well, because the compiler cannot check it. For those values we use `make` instead.

## Validating Run-Time Values

The `make` and `makeAll` constructors check the assertion at run time. They return a `Validation` that is either a success with a valid value or a failure with an error message explaining why the value was rejected.

To validate a value that is only known at run time:

```scala mdoc
val a = 10

val validatedSequenceNumber: Validation[String, SequenceNumber] =
  SequenceNumber.make(a)
```

To validate a whole collection of run-time values, we use `makeAll`. It applies the same assertion to each element and returns a single `Validation` over the collection, so either every element is valid or the result is a failure.

## Using a More Complex Assertion

An assertion can express much more than a range check. For example, `matches` accepts a regular expression and can validate a string such as an email address:

```scala mdoc:silent:reset
import zio.prelude.{Subtype, Validation}
import zio.prelude.Assertion._

object EmailAddress extends Subtype[String] {
  override def assertion = assert {
    matches(".+@.+\\..+")
  }
}
type EmailAddress = EmailAddress.Type
```

```scala mdoc
EmailAddress.make("reader@example.com")

EmailAddress.make("not an email")
```

## Bypassing the Check Inside the Object

The `wrap` operator converts a value to the new type without checking the assertion. In Scala 2 it is `protected`, so only code inside the new type object can call it. This lets the object's own operators create values that they know are valid, even when the compiler cannot prove it.

For example, the `next` operator on a sequence number cannot produce a negative value when the input is valid, so it can skip the check:

```scala mdoc:silent:reset
import zio.prelude.{Subtype, Validation}
import zio.prelude.Assertion._

object SequenceNumber extends Subtype[Int] {
  override def assertion = assert {
    greaterThanOrEqualTo(0)
  }

  implicit final class SequenceNumberOps(private val self: SequenceNumber) {
    def next: SequenceNumber =
      wrap(self + 1)
  }
}
type SequenceNumber = SequenceNumber.Type
```

When we know a value is valid at the call site, we can expose a constructor that skips the check. The caller takes responsibility for the value being valid:

```scala mdoc:silent:reset
import zio.prelude.{Subtype, Validation}
import zio.prelude.Assertion._

object SequenceNumber extends Subtype[Int] {
  override def assertion = assert {
    greaterThanOrEqualTo(0)
  }

  def unsafeMake(n: Int): SequenceNumber =
    SequenceNumber.wrap(n)
}
type SequenceNumber = SequenceNumber.Type
```

The `unsafeMake` name signals that no check runs. Use it only when the value has already been validated, for example when it comes from a trusted source or when performance matters.

## Combining the Pieces

A smart newtype can offer several constructors, each with its own trade-off. Compile-time `apply` gives the strongest guarantee for literals, `make` and `makeAll` handle run-time values safely, and `unsafeMake` skips the check when the caller already knows the value is valid. Together they give us full control over how a new type is created, without changing the underlying type.
