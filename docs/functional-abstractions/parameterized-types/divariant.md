---
id: divariant
title: "Divariant"
---

`Divariant` describes a binary type constructor, such as a function arrow, where the first parameter is contravariant and the second is covariant. The first parameter flips direction, so changing it requires a function in the opposite direction.

The standard example is a function `A => B`. We change its input by converting the new input type back to the old one, and we change its output by converting the old output to the new one. The combined operation, `dimap`, is the function-arrow version of `Bicovariant`'s `bimap`, except that the input side runs backwards.

## Changing Input and Output

`dimap` takes a function that converts the new input into the old input, and a function that converts the old output into the new output. It returns a function from the new input type to the new output type.

To build a function from `String` to `String` out of an `Int` to `Int` function, we convert the string to an `Int` on the way in and the `Int` back to a string on the way out:

```scala mdoc:silent:reset
import zio.prelude._

val doubleLength = implicitly[Divariant[Function1]].dimap(
  (s: String) => s.length,
  (i: Int) => i.toString
)

val transform: Int => Int = (i: Int) => i * 2
```

Next, we apply the transformed function to a string:

```scala mdoc
doubleLength(transform)("hello")
```

The string `"hello"` has five characters. It becomes `5`, which is doubled to `10`, and that becomes `"10"`.

## Changing Only One Side

`leftContramap` changes only the input side, and `rightMap` changes only the output side. Because the input is contravariant, `leftContramap` takes a function from the new input type to the old input type:

```scala mdoc
implicitly[Divariant[Function1]].leftContramap((s: String) => s.length).apply(transform)("abc")
```
