package zio.prelude

import cats.effect.unsafe.implicits.global
import cats.effect.{IO => CIO}
import cats.instances.list._
import cats.syntax.all._
import org.openjdk.jmh.annotations.{State => BenchmarkState, _}
import org.openjdk.jmh.infra.Blackhole
import zio.prelude.fx.ZPure
import zio.{Scope => _, _}

import java.util.concurrent.TimeUnit

@BenchmarkState(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 5, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 5, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 1)
class ForEachBenchmarks {

  var list: List[Int] = _

  @Param(Array("100000"))
  var size: Int = _

  @Setup(Level.Trial)
  def setup(): Unit =
    list = (1 to size).toList

  @Benchmark
  def catsTraverseOption(bh: Blackhole): Unit =
    bh.consume(list.traverse(Option(_)))

  @Benchmark
  def catsTraverse_Option(bh: Blackhole): Unit =
    bh.consume(list.traverse_(Option(_)))

  @Benchmark
  def catsTraverseFilterOption(bh: Blackhole): Unit =
    bh.consume(list.traverseFilter(a => Option(a.some)))

  @Benchmark
  def catsTraverseCIO(bh: Blackhole): Unit =
    bh.consume(list.traverse(CIO.pure(_)).unsafeRunSync())

  @Benchmark
  def catsTraverse_CIO(bh: Blackhole): Unit =
    bh.consume(list.traverse_(CIO.pure(_)).unsafeRunSync())

  @Benchmark
  def catsTraverseFilterCIO(bh: Blackhole): Unit =
    bh.consume(list.traverseFilter(a => CIO.pure(a.some)).unsafeRunSync())

  @Benchmark
  def zioForEachOption(bh: Blackhole): Unit =
    bh.consume(list.forEach(Option(_)))

  @Benchmark
  def zioForEach_Option(bh: Blackhole): Unit =
    bh.consume(list.forEach_(Option(_)))

  @Benchmark
  def zioForEachFilterOption(bh: Blackhole): Unit =
    bh.consume(list.forEachFilter(a => Option(Some(a))))

  @Benchmark
  def zioForEachZIO(bh: Blackhole): Unit =
    bh.consume(unsafeRun(list.forEach(ZIO.succeed(_))))

  @Benchmark
  def zioForEach_ZIO(bh: Blackhole): Unit =
    bh.consume(unsafeRun(list.forEach_(ZIO.succeed(_))))

  @Benchmark
  def zioForEachFilterZIO(bh: Blackhole): Unit =
    bh.consume(unsafeRun(list.forEachFilter(a => ZIO.succeed(Some(a)))))

  @Benchmark
  def zioForEachZPure(bh: Blackhole): Unit =
    bh.consume(list.forEach(ZPure.succeed[Unit, Int](_)).run)

  @Benchmark
  def zioForEach_ZPure(bh: Blackhole): Unit =
    bh.consume(list.forEach_(ZPure.succeed[Unit, Int](_)).run)

  @Benchmark
  def zioForEachFilterZPure(bh: Blackhole): Unit =
    bh.consume(list.forEachFilter(a => ZPure.succeed[Unit, Option[Int]](Some(a))).run)

  def unsafeRun[E, A](zio: ZIO[Any, E, A]): A =
    Unsafe.unsafe(implicit unsafe => Runtime.default.unsafe.run(zio).getOrThrowFiberFailure())
}
