---
id: running-and-converting
title: "Running and Converting ZPure"
---

A `ZPure` value describes a computation, and nothing happens until we run it. This guide covers the operators that run a computation and return its results, the operators that convert it to a `ZIO` value, and the stack safety that lets long computations run without overflowing the stack. For the capabilities that these operators return, see [Errors and Context](./errors-and-context.md) and [State and Logging](./state-and-logging.md).

Each run operator has requirements on the types of the computation. The state it starts with must be `Unit` if we do not pass an initial state, the environment must be provided, and the error type must be `Nothing` if the operator does not return errors. The compiler checks these requirements when we call the operator.

We use the following computation, which needs no environment, uses no state, and can not fail:

```scala mdoc:silent:reset
import zio.prelude._
import zio.prelude.fx.ZPure

val one: ZPure[Nothing, Unit, Unit, Any, Nothing, Int] =
  ZPure.succeed(1)

val two: ZPure[Nothing, Unit, Unit, Any, Nothing, Int] =
  ZPure.succeed(2)

val three: ZPure[Nothing, Unit, Unit, Any, Nothing, Int] =
  one.zipWith(two)(_ + _)
```

## Running for the Result

The simplest operator is `run` with no arguments. It is available when the state is `Unit`, the environment is provided, and the error type is `Nothing`, and it returns the value the computation produces:

```scala mdoc
three.run
```

When the computation starts with a state, we pass the initial state to `run`. The result is a pair of the final state and the value:

```scala mdoc:silent
val withState: ZPure[Nothing, Int, Int, Any, Nothing, String] =
  ZPure.modify[Int, Int, String](s => (s.toString, s + 1))
```

```scala mdoc
withState.run(41)
```

`runState` returns only the final state, and `runResult` returns only the value:

```scala mdoc
withState.runState(41)

withState.runResult(41)
```

## Running When Computations Can Fail

Computations with an error type can fail, so their results must include the error. `runEither` returns an `Either` with the error on the left and the value on the right. It requires an initial state of `Unit`:

```scala mdoc:silent
val failing: ZPure[Nothing, Unit, Unit, Any, String, Int] =
  ZPure.fail("boom")
```

```scala mdoc
failing.runEither
```

`runAll` is the most general operator. It takes the initial state and returns the log together with either the error or the final state and the value:

```scala mdoc
failing.runAll(())
```

`runValidation` returns a `ZValidation` that holds both the log and the result, which is useful when we want to keep the log of a computation that failed.

## Running for the Log

When the computation can not fail, `runLog` returns the log and the value:

```scala mdoc:silent
val logged: ZPure[String, Unit, Unit, Any, Nothing, Int] =
  ZPure.log("starting") *> ZPure.succeed(42)
```

```scala mdoc
logged.runLog
```

## Converting to ZIO

To use a `ZPure` computation inside an effectful `ZIO` program, we convert it with one of the `toZIO` operators. Each conversion returns a `ZIO` value that reads the environment from the `ZIO` environment, runs the `ZPure` computation, and succeeds or fails with the result. The conversion does not run the computation, so the examples below only show the types of the results.

`toZIO` converts a computation whose initial state is `Unit`. The resulting `ZIO` succeeds with the value or fails with the error:

```scala mdoc:compile-only
val asZIO: zio.ZIO[Any, Nothing, Int] =
  three.toZIO
```

`toZIOWith` takes the initial state. It has the same result type as `toZIO`:

```scala mdoc:compile-only
val withStateZIO: zio.ZIO[Any, Nothing, String] =
  withState.toZIOWith(41)
```

`toZIOWithState` also returns the final state along with the value:

```scala mdoc:compile-only
val withStateAndResultZIO: zio.ZIO[Any, Nothing, (Int, String)] =
  withState.toZIOWithState(41)
```

`toZIOWithAll` returns the log, the final state, and the value, so nothing that the computation produced is lost in the conversion:

```scala mdoc:compile-only
val everythingZIO: zio.ZIO[Any, Nothing, (zio.Chunk[String], Int, String)] =
  withState.toZIOWithAll(41)
```

## Stack Safety

A `ZPure` computation can be recursive without overflowing the call stack, because the runner keeps its own stack of pending steps instead of using the JVM call stack. The following computation recurses 100,000 times:

```scala mdoc
def countDown(n: Int): ZPure[Nothing, Unit, Unit, Any, Nothing, Int] =
  if (n == 0) ZPure.succeed(0)
  else ZPure.unit.flatMap(_ => countDown(n - 1))

countDown(100000).run
```

The result shows that the computation finished, even though each step nested inside the previous one.
