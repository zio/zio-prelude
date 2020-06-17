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

  type CatsIntState[A] = CatsState[Int, A]

  def catsLeftAssociatedBind(bound: Int): Int = {
    def loop(i: Int): CatsState[Int, Int] =
      if (i > bound) CatsState.pure(i)
      else CatsState.pure(i + 1).flatMap(loop)

    loop(0).runS(0).value
  }

  def catsGetSet(bound: Int): Int = {
    def loop(i: Int, acc: CatsState[Int, Int]): CatsState[Int, Int] =
      if (i > bound) acc.flatMap(_ => CatsState.set(i)).flatMap(_ => CatsState.get)
      else loop(i + 1, acc.flatMap(_ => CatsState.set(i)).flatMap(_ => CatsState.get))

    loop(0, CatsState.pure(0)).runS(0).value
  }

  def catsEffectfulTraversal(bound: Int): Int =
    (0 to bound).toList.traverse[CatsIntState, Unit] { el =>
      CatsState.get[Int].flatMap(s => CatsState.set(s + el))
    }.runS(0).value

  def zioLeftAssociatedBindTailRec(bound: Int): Int = {
    def loop(i: Int): State[Int, Int] =
      if (i > bound) State.succeed(i)
      else State.succeed(i + 1).flatMap(loop)

    loop(0).runState(0)
  }

  def zioGetSetTailRec(bound: Int): Int = {
    def loop(i: Int, acc: State[Int, Int]): State[Int, Int] =
      if (i > bound) acc.flatMap(_ => State.set(i)).flatMap(_ => State.get)
      else loop(i + 1, acc.flatMap(_ => State.set(i)).flatMap(_ => State.get))

    loop(0, State.succeed(0)).runState(0)
  }

  def zioEffectfulTraversalTailRec(bound: Int): Int =
    State.foreach((0 to bound).toList) { el =>
      State.get[Int].flatMap(s => State.set(s + el))
    }.runState(0)

  @Benchmark
  def catsLeftAssociatedBind1K(): Int =
    catsLeftAssociatedBind(1000)
  @Benchmark
  def catsLeftAssociatedBind10K(): Int =
    catsLeftAssociatedBind(10000)
  @Benchmark
  def catsLeftAssociatedBind100K(): Int =
    catsLeftAssociatedBind(100000)
  @Benchmark
  def catsLeftAssociatedBind1M(): Int =
    catsLeftAssociatedBind(1000000)

  @Benchmark
  def catsGetSet1K(): Int =
    catsGetSet(1000)
  @Benchmark
  def catsGetSet0K(): Int =
    catsGetSet(10000)
  @Benchmark
  def catsGetSet100K(): Int =
    catsGetSet(100000)
  @Benchmark
  def catsGetSet1M(): Int =
    catsGetSet(1000000)

  @Benchmark
  def catsEffectfulTraversal1K(): Int =
    catsEffectfulTraversal(1000)
  @Benchmark
  def catsEffectfulTraversal10K(): Int =
    catsEffectfulTraversal(10000)
  @Benchmark
  def catsEffectfulTraversal100K(): Int =
    catsEffectfulTraversal(100000)
  @Benchmark
  def catsEffectfulTraversal1M(): Int =
    catsEffectfulTraversal(1000000)

  @Benchmark
  def zioLeftAssociatedBindTailRec1K(): Int =
    zioLeftAssociatedBindTailRec(1000)
  @Benchmark
  def zioLeftAssociatedBindTailRec10K(): Int =
    zioLeftAssociatedBindTailRec(10000)
  @Benchmark
  def zioLeftAssociatedBindTailRec100K(): Int =
    zioLeftAssociatedBindTailRec(100000)
  @Benchmark
  def zioLeftAssociatedBindTailRec1M(): Int =
    zioLeftAssociatedBindTailRec(1000000)

  @Benchmark
  def zioGetSetTailRec1K(): Int =
    zioGetSetTailRec(1000)
  @Benchmark
  def zioGetSetTailRec10K(): Int =
    zioGetSetTailRec(10000)
  @Benchmark
  def zioGetSetTailRec100K(): Int =
    zioGetSetTailRec(100000)
  @Benchmark
  def zioGetSetTailRec1M(): Int =
    zioGetSetTailRec(1000000)

  @Benchmark
  def zioEffectfulTraversalTailRec1K(): Int =
    zioEffectfulTraversalTailRec(1000)
  @Benchmark
  def zioEffectfulTraversalTailRec10K(): Int =
    zioEffectfulTraversalTailRec(10000)
  @Benchmark
  def zioEffectfulTraversalTailRec100K(): Int =
    zioEffectfulTraversalTailRec(100000)
  @Benchmark
  def zioEffectfulTraversalTailRec1M(): Int =
    zioEffectfulTraversalTailRec(1000000)
}