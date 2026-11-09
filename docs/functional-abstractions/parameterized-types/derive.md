---
id: derive
title: "Derive"
---

`Derive[F, Typeclass]` is a recipe for building a type class instance for a container type from an instance for its elements. The type constructor `F` is the container, such as `List`, and `Typeclass` is the type class, such as `Equal`.

For example, if we know how to compare two `Int` values for equality, we can compare two `List[Int]` values by checking that they have the same length and that each pair of corresponding elements is equal. That rule works for any element type that has an `Equal` instance, so it can be written once as a `Derive` instance.

The library uses `Derive` internally. It gives `List`, `Chunk`, `Option`, and other containers their `Equal` instances this way, so we rarely need to write one ourselves.

## Using a Derive Instance

`Derive.apply` summons the derive instance for a container and a type class. Its `derive` method takes an instance for the element type and returns an instance for the container:

```scala mdoc:silent:reset
import zio.prelude._
```

```scala mdoc
Derive[List, Equal].derive[Int].equal(List(1, 2), List(1, 2))

Derive[List, Equal].derive[Int].equal(List(1, 2), List(2, 1))
```

The same instance compares lists of any element type that has an `Equal` instance, such as strings:

```scala mdoc
Derive[List, Equal].derive[String].equal(List("a"), List("a"))
```

## Defining Your Own Derive Instance

A derive instance is a value of type `Derive[F, Typeclass]` with a `derive` method. It is useful for a container that you define yourself, when you want every instance of the container to follow the same rule. The rule should be written once in terms of the element instance, and it must be valid for every element type the type class applies to.
