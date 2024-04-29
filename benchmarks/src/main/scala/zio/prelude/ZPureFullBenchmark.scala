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
@Measurement(iterations = 3, timeUnit = TimeUnit.SECONDS, time = 1)
@Warmup(iterations = 3, timeUnit = TimeUnit.SECONDS, time = 1)
@Fork(value = 2)
class ZPureFullBenchmark {

  var list: List[Int] = _

  @Param(Array("1000"))
  var size: Int = _

  @Setup(Level.Trial)
  def setup(): Unit =
    list = (1 to size).toList

  @Benchmark
  def zioPureFullBenchmark(bh: Blackhole): Unit = bh.consume(run0)

  private def run0 =
    ZPure
      .foreachDiscard(list)(_ =>
        for {
          conf <- ZPure.environmentWith[Env](_.get.config)
          event = Event(s"Env = $conf")
          _    <- ZPure.log(event)
          add   = 1
          _    <- ZPure.update[State, State](state => state.copy(value = state.value + add))
        } yield ()
      )
      .provideService(Env())
      .run(State())

  private case class Env(config: String = "foo")
  private case class Event(value: String)
  private case class State(value: Int = 0)

}
