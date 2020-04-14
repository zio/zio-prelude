package zio.prelude

import zio.test._
import zio.test.laws._
import zio.random.Random

object CovariantSpec extends DefaultRunnableSpec {

  val OptionGenF: GenF[Random, Option] =
    new GenF[Random, Option] {
      def apply[R1 <: Random, A](gen: Gen[R1, A]): Gen[R1, Option[A]] =
        Gen.option(gen)
    }

  def spec = suite("CovariantSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(Covariant)(OptionGenF, Gen.anyInt))
    )
  )
}
