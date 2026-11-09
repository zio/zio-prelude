---
id: recursive
title: "Recursive"
---

`Recursive[Case, Annotations]` represents a recursive data structure, such as an expression tree or a list, without writing the recursion by hand. The `Case` type parameter is a type constructor that describes one layer of the structure, and `Recursive` supplies the nesting.

For example, a small expression language has literals and additions. A layer of that language refers to its sub-expressions through a type parameter, and `Recursive` plugs the layers together. The benefit is that operators such as `fold` can process the whole structure without knowing its shape in advance.

We define the layer type, with a case for each kind of node. The type parameter `A` stands for the sub-expressions:

```scala mdoc:silent:reset
import zio._
import zio.prelude._
import zio.prelude.recursive._

sealed trait ExprF[+A]
final case class Lit(value: Int) extends ExprF[Nothing]
final case class Add[A](left: A, right: A) extends ExprF[A]
```

`Recursive` needs a `Covariant` instance for the layer type. This tells it how to transform the sub-expressions while keeping the layer's shape:

```scala mdoc:silent
implicit val exprCovariant: Covariant[ExprF] =
  new Covariant[ExprF] {
    def map[A, B](f: A => B): ExprF[A] => ExprF[B] = {
      case Lit(value)       => Lit(value)
      case Add(left, right) => Add(f(left), f(right))
    }
  }
```

We build an expression tree by wrapping each layer in `Recursive`. The annotations here are empty, so we use an empty `ZEnvironment`:

```scala mdoc:silent
def lit(value: Int): Recursive[ExprF, Any] =
  Recursive(Lit(value), ZEnvironment.empty)

def add(left: Recursive[ExprF, Any], right: Recursive[ExprF, Any]): Recursive[ExprF, Any] =
  Recursive(Add(left, right), ZEnvironment.empty)

val expression: Recursive[ExprF, Any] =
  add(lit(1), add(lit(2), lit(3)))
```

## Folding a Recursive Structure

`fold` processes the structure from the bottom up. It takes a function that receives one layer whose sub-expressions have already been folded, and returns a result for that layer. To evaluate the expression, each literal becomes its value and each addition becomes the sum of its two sides:

```scala mdoc
expression.fold[Int] {
  case Lit(value)       => value
  case Add(left, right) => left + right
}
```

A `Folder` packages the same function so that it can be reused and passed around as a value. `Folder[ExprF, Int]` takes a layer of `Int` results and returns an `Int`:

```scala mdoc
val evaluate: Folder[ExprF, Int] =
  Folder[ExprF, Int] {
    case Lit(value)       => value
    case Add(left, right) => left + right
  }

expression.fold(evaluate.apply)
```

`Unfolder` is the reverse. It takes a seed value and produces one layer, whose sub-expressions are seeds for the next step. It is used to build recursive structures from simpler data.
