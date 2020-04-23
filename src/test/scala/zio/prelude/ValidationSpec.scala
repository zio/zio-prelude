package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.laws._

object ValidationSpec extends DefaultRunnableSpec {

  val genValidation: Gen[Random with Sized, Validation[Int, Int]] =
    Gens.validation(Gen.anyInt, Gen.anyInt)

  val genFValidation: GenF[Random with Sized, ({ type lambda[+x] = Validation[Int, x] })#lambda] =
    GenFs.validation(Gen.anyInt)

  type IntValidation[+A] = Validation[Int, A]

  def spec = suite("ValidationSpec")(
    suite("laws")(
      testM("associativeBothF")(checkAllLaws(AssociativeBothF)(genFValidation, Gen.anyInt)),
      testM("commutativeBothF")(checkAllLaws(CommutativeBothF)(genFValidation, Gen.anyInt)),
      testM("covariant")(checkAllLaws(Covariant)(genFValidation, Gen.anyInt)),
      testM("equal")(checkAllLaws(Equal)(genValidation)),
      testM("hash")(checkAllLaws(Hash)(genValidation)),
      testM("identityBothF")(checkAllLaws(IdentityBothF)(genFValidation, Gen.anyInt)),
      testM("ord")(checkAllLaws(Ord)(genValidation))
    )
  )
}
