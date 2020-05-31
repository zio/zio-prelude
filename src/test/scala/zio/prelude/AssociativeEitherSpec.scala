package zio.prelude

import zio.test._
import zio.test.laws._
import zio.random.Random

object AssociativeEitherSpec extends DefaultRunnableSpec {

  val genFEither: GenF[Random with Sized, ({ type lambda[+x] = Either[Int, x] })#lambda] =
    GenFs.either(Gen.anyInt)

  def spec = suite("AssociativeEitherSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(AssociativeEither)(GenFs.option, Gen.anyInt)),
      testM("either")(checkAllLaws(AssociativeEither)(genFEither, Gen.anyInt))
    )
  )
}
