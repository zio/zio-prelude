package zio.prelude

import zio.random.Random
import zio.test.Gen

import scala.concurrent.duration.{Duration => ScalaDuration}
import scala.jdk.DurationConverters._

object Common {
  def anyFiniteDurationScala: Gen[Random, ScalaDuration] = Gen.anyFiniteDuration.map(_.toScala)
}
