---
id: zivariant
title: "Zivariant"
---

`Zivariant` describes a type constructor with three parameters: an environment `R` that is contravariant, and an error `E` and a value `A` that are both covariant. ZIO effects such as `ZIO[R, E, A]` have this shape, so `Zivariant` describes how to transform an effect's environment, error, and value.

It is the three-parameter version of the earlier abstractions. `Bicovariant` maps two covariant parameters, and `Divariant` maps a contravariant input and a covariant output. `Zivariant` combines the two, because the environment works like a contravariant input and the error and value work like covariant outputs.

## Mapping Values and Errors

`map` transforms the success value, and `mapLeft` transforms the error. Both return a function from the original effect to the new one:

```scala mdoc:silent:reset
import zio._
import zio.prelude._
```

```scala mdoc:compile-only
val succeeded: ZIO[Any, String, Int] = ZIO.succeed(20)

val mapped: ZIO[Any, String, Int] =
  implicitly[Zivariant[ZIO]].map((i: Int) => i + 1).apply(succeeded)

val failedWithLength: ZIO[Any, Int, Int] =
  implicitly[Zivariant[ZIO]].mapLeft((s: String) => s.length).apply(ZIO.fail("boom"))
```

## Transforming the Environment

`contramap` changes the environment that an effect requires. The function takes the new environment and returns the old one, which is why it runs in the opposite direction:

```scala mdoc:compile-only
val needsNumber: ZIO[Int, String, Int] =
  ZIO.serviceWith[Int](_ * 2)

val needsString: ZIO[String, String, Int] =
  implicitly[Zivariant[ZIO]].contramap((env: ZEnvironment[String]) => ZEnvironment(env.get[String].length)).apply(needsNumber)
```

`Zivariant` has instances for `ZIO`, `ZStream`, and `ZSTM`, so the same operators apply to each of them.
