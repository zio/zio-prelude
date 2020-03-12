package zio.prelude

import zio.random.Random
import zio.test.{ Gen, Sized }

object TestUtil {
  def anyMap[R <: Random with Sized, A, B](a: Gen[R, A], b: Gen[R, B]): Gen[R, Map[A, B]] =
    Gen.listOf(a.zip(b)).map(_.toMap)

  def anySet[R <: Random with Sized, A](gen: Gen[R, A]): Gen[R, Set[A]] =
    Gen.listOf(gen).map(_.toSet)
}
