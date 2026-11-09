---
id: optional
title: "Optional"
---

`Optional[A]` describes a value that may be absent. It plays the same role as the standard `Option` type, and it has the same two cases: a value that is present and a value that is absent.

The difference lies in what the type works with. `Optional` is a data type from ZIO Prelude, so it has instances for the functional abstractions in the library. Code written against those abstractions handles an optional value without special cases.

We import `Optional` from the `zio.prelude.data` package:

```scala mdoc:silent:reset
import zio.prelude.data.Optional
```

## Present and Absent Values

An `Optional` is either `Optional.Present`, which holds a value, or `Optional.Absent`, which holds nothing:

```scala mdoc
val present: Optional[Int] =
  Optional.Present(42)

val absent: Optional[Int] =
  Optional.Absent
```

Scala converts values to `Optional` implicitly, so a plain value works where an `Optional` is required. A value becomes `Present`, and an `Option` becomes `Present` when it holds a value and `Absent` when it is `None`:

```scala mdoc
val fromValue: Optional[String] =
  "hello"

val fromOption: Optional[Int] =
  Option.empty[Int]
```

## Working With an Optional Value

The operators on `Optional` mirror the operators on `Option`. `map` and `flatMap` transform a present value and leave an absent value unchanged:

```scala mdoc
present.map(_ + 1)

absent.map(_ + 1)
```

`getOrElse` returns the value if it is present and the default otherwise. `fold` takes one function for the absent case and one for the present case:

```scala mdoc
absent.getOrElse(0)

present.fold("nothing")(value => s"found $value")
```

Converting to other types uses the names the standard library uses. `toOption` returns a standard `Option`, and `toLeft` and `toRight` convert to an `Either`:

```scala mdoc
present.toOption

present.toRight("missing")
```

`filter`, `exists`, `forall`, and `contains` check a present value against a condition. An absent value fails each of these checks:

```scala mdoc
present.filter(_ > 40)

absent.exists(_ > 40)
```
