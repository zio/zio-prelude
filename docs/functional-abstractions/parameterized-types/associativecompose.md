---
id: associativecompose
title: "AssociativeCompose"
---

`AssociativeCompose` describes a way to chain two transformations, where the transformation from the first input to the second output is itself formed by chaining. Functions are the standard example: given a function from `A` to `B` and a function from `B` to `C`, composing them gives a function from `A` to `C`.

The type constructor is parameterized over a binary type constructor `=>:`, which plays the role of the arrow. For plain functions, that is `Function1`. The type class needs only two operations, `compose` and `associativeCompose`, and the rule that composing is associative: chaining three transformations gives the same result however the chain is grouped.

## Composing Functions

`compose` takes the function to apply second and the function to apply first. For functions, it returns a function that applies the first one and then the second. The function applied first takes a `String` and returns an `Int`, and the function applied second takes that `Int` and returns a new `Int`:

```scala mdoc:silent:reset
import zio.prelude._

val lengthThenDouble: String => Int =
  implicitly[AssociativeCompose[Function1]].compose((n: Int) => n * 2, (s: String) => s.length)
```

```scala mdoc
lengthThenDouble("hello")
```

The result doubles the length of `"hello"`, which is ten.

## Associativity

Because composition is associative, a chain of three functions gives the same result whichever pair we group first. Any instance of `AssociativeCompose` must satisfy this rule, so the grouping of a chain never changes its result.

`associativeCompose` is the same operation written in the form that makes the grouping explicit, which is useful when we want to show the rule itself.
