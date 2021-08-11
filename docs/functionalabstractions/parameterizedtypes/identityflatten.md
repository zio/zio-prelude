---
id: identityflatten
title: "IdentityFlatten"
---

`IdentityFlatten[F]` describes a way of combining two layers of a value of type `F[F[A]]` into a `F[A]` in a way that is associative and has an identity value `any` of type `F[Any]`.

Its signature is:

```scala mdoc
trait AssociativeFlatten[F[+_]] {
  def flatten[A](f: F[F[A]]): F[A]
}

trait IdentityFlatten[F[+_]] extends AssociativeFlatten[F] {
  def any: F[Any]
}
```

The `any` value must be an identity with respect to the `flatten` operator. So if we add a new layer of nesting by mapping a value into the `any` value and then remove that layer with `flatten` we should get the original value back unchanged.

```scala
fa.map(a => any.map(_ => a)).flatten === fa
any.map(_ => fa).flatten === fa

```

This may look slightly different than the normal identity law because we have to map over the value but it is the same as the identity laws for the `Identity`, `IdentityBoth` and `IdentityEither` abstractions. Note how the two laws say whether we add the additional layer on the inside or the outside doesn't matter.

To be an identity element, the `any` value must not do anything when run other than return a value with no information content. This way we can always add a layer and remove it by using `flatten` to get back to the original value.

For example the `any` value for `ZIO` is `unit`. The `unit` workflow doesn't do anything and always succeeds.

So we can always `flatten` a `ZIO` workflow constructed by mapping over the `unit` value to the original workflow back.

```scala mdoc:reset
import zio._

import java.io.IOException

val helloIdentity: ZIO[Has[Console], IOException, Unit] =
  ZIO.unit.map { _ =>
    Console.printLine("Hello from an identity!")
  }.flatten
```

Recall from our discussion of `AssociativeFlatten` that the `flatten` operator corresponds to running a workflow, which returns a new workflow, and then running the workflow. Here the `unit` workflow doesn't do anything and always succeeds so we can simply eliminate it.

Just like the `any` value that is an identity with respect to the `both` operator of a `IdentityBoth`, the `any` value represents a successful value that contains no information. So other examples of the `any` value would include:

```scala mdoc
val anyOption: Option[Any] =
  Some(())

val anyEither: Either[Nothing, Any] =
  Right(())

val anyList: List[Any] =
  List(())
```

By combining the structure described by the `IdentityFlatten` abstraction and the `Covariant` abstraction we can also define the `succeed` operator we saw from `IdentityBoth`.

```scala mdoc
import zio.prelude._

def succeed[F[+_]: IdentityFlatten : Covariant, A](a: => A): F[A] =
  IdentityFlatten[F].any.map(_ => a)
```

As discussed in the section on `IdentityBoth`, the `succeed` operator is helpful because it lets us "lift" any normal value into the context of the parameterized type.

For example, using the `succeed` operator on `ZIO` we can import an arbitrary block of Scala code into a `ZIO` workflow.

```scala mdoc
def helloScala: ZIO[Any, Nothing, Unit] =
  ZIO.succeed(println("Hello Scala!"))
```

For this operator to be well defined we have to be able to construct a value `any` that doesn't do anything and has no information content. If all values of the parameterized type required some additional information, for example, we would not be able to define the `IdentityFlatten` abstraction or `succeed` for it.

In addition to the `succeed` operator being generally useful, there are some other operators that require an `IdentityFlatten` instance so that they can use it in their own implementations.

In particular, the `ForEach` abstraction defines a set of operators that allow us to fold over a collection in the context of a parameterized type. These require that we have an `IdentityFlatten` instance so that if the collection is empty we can simply succeed with an empty collection in the context of the parameterized type.

The `IdentityFlatten` abstraction is quite powerful, combining the flexibility of the `AssociativeFlatten` abstraction to run one value and then use its result to run another value with the ability to lift arbitrary values into the context of the parameterized type.

If you are using data types from ZIO or the standard library there are some operators defined on the `ForEach` abstraction that can be helpful but otherwise the operators defined in terms of `IdentityFlatten` are likely already implemented directly on the data types you are using.

If you are implementing your own data type you should certainly consider whether your data type can implement `flatMap`, or whether you explicitly don't want to. Either way being able to lift a value into the parameterized type with `succeed` using either the `IdentityBoth` or `IdentityFlatten` abstraction is extremely useful so you should either implement one of those abstractions or have very specific reasons why that is not possible.

Finally, for writing generic code the combination of the `IdentityFlatten` and `Covariant` abstractions is also quite powerful, allowing modeling imperative code in a functional context.
