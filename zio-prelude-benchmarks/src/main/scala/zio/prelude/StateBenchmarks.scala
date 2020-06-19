package zio.prelude

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{ State => BenchmarkState, _ }

import cats.data.{ State => CatsState }
import cats.instances.list._
import cats.syntax.traverse._

@BenchmarkState(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class StateBenchmarks {

  var list: List[Int] = _

  @Param(Array("1000", "10000", "100000", "1000000"))
  var size: Int = _

  @Setup(Level.Trial)
  def setup() =
    list = (1 to size).toList

  type CatsIntState[A] = CatsState[Int, A]

  @Benchmark
  def catsLeftAssociatedBind(): Int = {
    def loop(i: Int): CatsState[Int, Int] =
      if (i > size) CatsState.pure(i)
      else CatsState.pure(i + 1).flatMap(loop)

    loop(0).runS(0).value
  }

  @Benchmark
  def catsGetSet(): Int = {
    def loop(i: Int, acc: CatsState[Int, Int]): CatsState[Int, Int] =
      if (i > size) acc.flatMap(_ => CatsState.set(i)).flatMap(_ => CatsState.get)
      else loop(i + 1, acc.flatMap(_ => CatsState.set(i)).flatMap(_ => CatsState.get))

    loop(0, CatsState.pure(0)).runS(0).value
  }

  @Benchmark
  def catsEffectfulTraversal(): Int =
    list
      .traverse[CatsIntState, Unit] { el =>
        CatsState.get[Int].flatMap(s => CatsState.set(s + el))
      }
      .runS(0)
      .value

  @Benchmark
  def zioLeftAssociatedBindTailRec(): Int = {
    def loop(i: Int): State[Int, Int] =
      if (i > size) State.succeed(i)
      else State.succeed(i + 1).flatMap(loop)

    loop(0).runState(0)
  }

  @Benchmark
  def zioGetSetTailRec(): Int = {
    def loop(i: Int, acc: State[Int, Int]): State[Int, Int] =
      if (i > size) acc.flatMap(_ => State.set(i)).flatMap(_ => State.get)
      else loop(i + 1, acc.flatMap(_ => State.set(i)).flatMap(_ => State.get))

    loop(0, State.succeed(0)).runState(0)
  }

  @Benchmark
  def zioEffectfulTraversalTailRec(): Int =
    State
      .foreach(list) { el =>
        State.get[Int].flatMap(s => State.set(s + el))
      }
      .runState(0)
}
