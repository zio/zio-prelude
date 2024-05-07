package zio.prelude

import org.openjdk.jmh.annotations.{State => BenchmarkState, _}
import org.openjdk.jmh.infra.Blackhole
import zio.prelude.fx.ZPure
import zio.{Scope => _}

import java.util.concurrent.TimeUnit

@BenchmarkState(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 3, timeUnit = TimeUnit.SECONDS, time = 1)
@Warmup(iterations = 3, timeUnit = TimeUnit.SECONDS, time = 1)
@Fork(value = 2)
class ZPureFullBenchmark {

  var list: List[Int] = _

  @Param(Array("1", "1000"))
  var size: Int = _

  @Setup(Level.Trial)
  def setup(): Unit =
    list = (1 to size).toList

  @Benchmark
  def fallibleBenchmark(bh: Blackhole): Unit = bh.consume(runFallible)

  @Benchmark
  def infallibleBenchmark(bh: Blackhole): Unit = bh.consume(runInfallible)

  private def runFallible =
    ZPure
      .foreachDiscard(list)(_ =>
        (for {
          conf <- ZPure.environmentWith[Env](_.get.config)
          _    <- ZPure.log(Event(s"Env = $conf"))
          add   = 1
          _    <- if (true) ZPure.unit[State] else ZPure.fail(new Throwable("boom"))
          _    <- ZPure.update[State, State](state => state.copy(value = state.value + add))
        } yield ()).catchAll(e => ZPure.log[State, Event](Event(e.toString)))
      )
      .provideService(Env())
      .run(State())

  private def runInfallible =
    ZPure
      .foreachDiscard(list)(_ =>
        (for {
          conf <- ZPure.environmentWith[Env](_.get.config)
          _    <- ZPure.log(Event(s"Env = $conf"))
          add   = 1
          _    <- ZPure.unit[State]
          _    <- ZPure.update[State, State](state => state.copy(value = state.value + add))
        } yield ()) *> ZPure.unit[State]
      )
      .provideService(Env())
      .run(State())

  private case class Env(config: String = "foo")
  private case class Event(value: String)
  private case class State(value: Int = 0)
}
