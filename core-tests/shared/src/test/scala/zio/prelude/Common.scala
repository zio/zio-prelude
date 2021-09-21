package zio.prelude

import zio._
import zio.test.Gen

import scala.concurrent.duration.{Duration => ScalaDuration}

object Common {
  def finiteDurationScala: Gen[Has[Random], ScalaDuration] =
    Gen.long(0L, Long.MaxValue / 10).map(ScalaDuration.fromNanos)
}
