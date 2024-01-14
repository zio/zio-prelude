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
class CollectBenchmarks {

  var list: List[Int] = _

  @Param(Array("100000"))
  var size: Int = _

  @Setup(Level.Trial)
  def setup(): Unit =
    list = (1 to size).toList

  @Benchmark
  def catsTraverseFilterOption(bh: Blackhole): Unit =
    bh.consume(list.traverse(n => Option(Option(n))))

  @Benchmark
  def catsTraverseFilterCIO(bh: Blackhole): Unit =
    bh.consume(list.traverseFilter(n => CIO.pure(Option(n))).unsafeRunSync())

  @Benchmark
  def zioCollectMOption(bh: Blackhole): Unit =
    bh.consume(list.collectM(n => Option(Option(n))))

  @Benchmark
  def zioCollectMZIO(bh: Blackhole): Unit =
    bh.consume(unsafeRun(list.collectM(n => ZIO.succeed(Option(n)))))

  @Benchmark
  def zioCollectMZPure(bh: Blackhole): Unit =
    bh.consume(list.collectM(n => ZPure.succeed[Unit, Option[Int]](Option(n))).run)

  def unsafeRun[E, A](zio: ZIO[Any, E, A]): A =
    Unsafe.unsafe(implicit unsafe => Runtime.default.unsafe.run(zio).getOrThrowFiberFailure())
}
