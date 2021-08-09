package zio.prelude

import zio.random.Random
import zio.test.Gen

import scala.concurrent.duration.{Duration => ScalaDuration}

object Common {
  def anyFiniteDurationScala: Gen[Random, ScalaDuration] = Gen.long(0L, Long.MaxValue / 10).map(ScalaDuration.fromNanos)
}
