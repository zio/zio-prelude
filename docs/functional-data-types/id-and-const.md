---
id: id-and-const
title: "Id and Const"
---

`Id` and `Const` are two new types that give simple, effect-free meaning to the type parameters of functional abstractions. They are rarely used directly. They appear when a function needs a type constructor, such as a `ForEach` operator, and the effect it uses should be trivial.

We import both from the `zio.prelude` package:

```scala mdoc:silent:reset
import zio.prelude._
```

## Id

`Id[A]` is a wrapper around `A` with no effect. It is the type constructor you get when you want an operator that expects an effect, but the effect should do nothing special.

Because `Id` has all the instances that such operators need, we can use it with `ForEach` to apply a function to each element of a `List` and get the result back as an `Id`:

```scala mdoc
val doubled: Id[List[Int]] =
  ForEach[List].forEach(List(1, 2, 3))(a => Id(a * 2))
```

`Id.unwrap` returns the plain `List` inside the wrapper:

```scala mdoc
Id.unwrap(doubled)
```

The wrapper adds nothing at run time. `Id` is a newtype, so its values are the same as the underlying `A` values.

## Const

`Const[A, B]` holds a value of type `A` and ignores the second type parameter `B`. The `B` parameter is a phantom type, which means it does not appear in the value. It only appears in the type, so that `Const` can fit a position that expects a type constructor with one parameter.

Because `B` is never used, the same `Const` value can stand in for a `Const` of any `B`. To construct a value, we use `Const.apply`:

```scala mdoc
val label: Const[String, Int] =
  Const("name")

Const.unwrap(label)
```

Like `Id`, `Const` is a newtype, so it adds no allocation at run time. Its `A` value is the underlying value.
