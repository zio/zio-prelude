---
id: identitycompose
title: "IdentityCompose"
---

`IdentityCompose` extends [AssociativeCompose](./associativecompose.md) with an identity arrow. For functions, the identity is the function that returns its input unchanged. Composing any function with the identity gives back that same function.

The identity arrow is what makes the operation a monoid-like structure, the same way the number zero is the identity for addition. Chaining a transformation with nothing at all leaves it unchanged.

## The Identity Arrow

`IdentityCompose.identity` returns the identity for the type constructor. For `Function1`, it is the function that returns its argument:

```scala mdoc:silent:reset
import zio.prelude._
```

We first look at the identity arrow on a number:

```scala mdoc
implicitly[IdentityCompose[Function1]].identity[Int].apply(5)
```

## Composing With the Identity

Composing a function with the identity on either side gives the same function back. Here we compose with the identity on the right, so the identity runs first and changes nothing:

```scala mdoc:silent
val lengthThenDouble: String => Int =
  implicitly[AssociativeCompose[Function1]].compose((n: Int) => n * 2, (s: String) => s.length)


val unchanged: String => Int =
  implicitly[AssociativeCompose[Function1]].compose(lengthThenDouble, implicitly[IdentityCompose[Function1]].identity[String])

```

Next, we compose a function with the identity and apply the result to a string:

```scala mdoc
unchanged("hello")
```

The result equals `lengthThenDouble` on `"hello"`, which is ten.
