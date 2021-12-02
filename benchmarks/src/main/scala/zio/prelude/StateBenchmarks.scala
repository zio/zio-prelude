package zio.prelude

import cats.data.{EitherT, Kleisli, State => CatsState}
import cats.instances.list._
import cats.syntax.traverse._
import org.openjdk.jmh.annotations.{State => BenchmarkState, _}

import java.util.concurrent.TimeUnit

@BenchmarkState(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class StateBenchmarks {

  var list: List[Int] = _

  @Param(Array("100000"))
  var size: Int = _

  @Setup(Level.Trial)
  def setup(): Unit =
    list = (1 to size).toList

  type CatsIntState[A]       = CatsState[Int, A]
  type CatsEitherIntState[A] = EitherT[CatsIntState, Nothing, A]

  type CatsStack[A] = Kleisli[CatsEitherIntState, Any, A]
  object CatsStack {
    def get: CatsStack[Int]                        = {
      val eitherT: CatsEitherIntState[Int] =
        EitherT.liftF[CatsIntState, Nothing, Int](CatsState.get)
      Kleisli.liftF(eitherT)
    }
    def pure[A](a: A): CatsStack[A]                =
      Kleisli.pure(a)
    def runState[A](fa: CatsStack[A])(s: Int): Int =
      fa.run(()).merge.runS(s).value
    def set(n: Int): CatsStack[Unit]               = {
      val eitherT: CatsEitherIntState[Unit] =
        EitherT.liftF[CatsIntState, Nothing, Unit](CatsState.set(n))
      Kleisli.liftF(eitherT)
    }
  }

  @Benchmark
  def catsStackLeftAssociatedBind(): Int = {
    def loop(i: Int): CatsStack[Int] =
      if (i > size) CatsStack.pure(i)
      else CatsStack.pure(i + 1).flatMap(loop)

    CatsStack.runState(loop(0))(0)
  }

  @Benchmark
  def catsStackGetSet(): Int = {
    def loop(i: Int, acc: CatsStack[Int]): CatsStack[Int] =
      if (i > size) acc.flatMap(_ => CatsStack.set(i)).flatMap(_ => CatsStack.get)
      else loop(i + 1, acc.flatMap(_ => CatsStack.set(i)).flatMap(_ => CatsStack.get))

    CatsStack.runState(loop(0, CatsStack.pure(0)))(0)
  }

  @Benchmark
  def catsStackEffectfulTraversal(): Int =
    CatsStack.runState(
      list.traverse[CatsStack, Unit] { el =>
        CatsStack.get.flatMap(s => CatsStack.set(s + el))
      }
    )(0)

  @Benchmark
  def catsStateLeftAssociatedBind(): Int = {
    def loop(i: Int): CatsState[Int, Int] =
      if (i > size) CatsState.pure(i)
      else CatsState.pure(i + 1).flatMap(loop)

    loop(0).runS(0).value
  }

  @Benchmark
  def catsStateGetSet(): Int = {
    def loop(i: Int, acc: CatsState[Int, Int]): CatsState[Int, Int] =
      if (i > size) acc.flatMap(_ => CatsState.set(i)).flatMap(_ => CatsState.get)
      else loop(i + 1, acc.flatMap(_ => CatsState.set(i)).flatMap(_ => CatsState.get))

    loop(0, CatsState.pure(0)).runS(0).value
  }

  @Benchmark
  def catsStateEffectfulTraversal(): Int =
    list
      .traverse[CatsIntState, Unit] { el =>
        CatsState.get[Int].flatMap(s => CatsState.set(s + el))
      }
      .runS(0)
      .value

  @Benchmark
  def zioLeftAssociatedBind(): Int = {
    def loop(i: Int): State[Int, Int] =
      if (i > size) State.succeed(i)
      else State.succeed(i + 1).flatMap(loop)

    loop(0).runState(0)
  }

  @Benchmark
  def zioGetSet(): Int = {
    def loop(i: Int, acc: State[Int, Int]): State[Int, Int] =
      if (i > size) acc.flatMap(_ => State.set(i)).flatMap(_ => State.get)
      else loop(i + 1, acc.flatMap(_ => State.set(i)).flatMap(_ => State.get))

    loop(0, State.succeed(0)).runState(0)
  }

  @Benchmark
  def zioEffectfulTraversal(): Int =
    State
      .forEach(list) { el =>
        State.get[Int].flatMap(s => State.set(s + el))
      }
      .runState(0)
}
