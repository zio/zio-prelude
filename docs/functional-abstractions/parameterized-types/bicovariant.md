---
id: bicovariant
title: "Bicovariant"
---

`Bicovariant` describes a type constructor with two type parameters that can be mapped covariantly in both. A value of type `F[A, B]` can have its first parameter mapped with a function from `A` to `AA`, its second parameter mapped with a function from `B` to `BB`, or both at once with `bimap`.

`Either` is the standard example. An `Either[A, B]` holds either an `A` on the left or a `B` on the right, and we can transform either side independently.

`Bicovariant` extends `RightCovariant`, which lets the second parameter be mapped on its own. Because of that, the same instance also gives a `Covariant` for the second parameter.

## Mapping Both Sides

`bimap` takes a function for each side and returns a function from the original `Either` to the transformed one. Here the left side becomes its length and the right side is doubled:

```scala mdoc:silent:reset
import zio.prelude._

val transform = implicitly[Bicovariant[Either]].bimap((s: String) => s.length, (i: Int) => i * 2)
```

```scala mdoc
transform(Left("hello"))

transform(Right(21))
```

## Mapping One Side

`leftMap` transforms only the first parameter, and `rightMap` transforms only the second. The other side is left alone:

```scala mdoc
implicitly[Bicovariant[Either]].leftMap((s: String) => s.toUpperCase).apply(Left("error"))

implicitly[Bicovariant[Either]].rightMap((i: Int) => i + 1).apply(Right(41))
```

`Tuple2` and `Exit` also have `Bicovariant` instances, so the same operators apply to pairs and to ZIO `Exit` values.
