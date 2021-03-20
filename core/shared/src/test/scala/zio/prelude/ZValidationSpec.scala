package zio.prelude

import zio.ZIO
import zio.prelude.Equal._
import zio.prelude.HashSpec.scalaHashCodeConsistency
import zio.prelude.ZValidation._
import zio.random.Random
import zio.test._
import zio.test.laws._

object ZValidationSpec extends DefaultRunnableSpec {

  val genValidation: Gen[Random with Sized, ZValidation[Int, Int, Int]] =
    Gens.validation(Gen.anyInt, Gen.anyInt, Gen.anyInt)

  val genFValidation: GenF[Random with Sized, ({ type lambda[+x] = ZValidation[Int, Int, x] })#lambda] =
    GenFs.validation(Gen.anyInt, Gen.anyInt)

  val genFValidationFailure
    : GenF[Random with Sized, ({ type lambda[+x] = newtypes.Failure[ZValidation[Int, x, Int]] })#lambda] =
    GenFs.validationFailure(Gen.anyInt, Gen.anyInt)

  def spec: ZSpec[Environment, Failure] = suite("ZValidationSpec")(
    suite("laws")(
      testM("associativeBoth")(checkAllLaws(AssociativeBoth)(genFValidation, Gen.anyInt)),
      testM("commutativeBoth")(checkAllLaws(CommutativeBoth)(genFValidation, Gen.anyInt)),
      testM("covariant")(checkAllLaws(Covariant)(genFValidation, Gen.anyInt)),
      testM("equal")(checkAllLaws(Equal)(genValidation)),
      testM("hash")(checkAllLaws(Hash)(genValidation)),
      testM("identityBoth")(checkAllLaws(IdentityBoth)(genFValidation, Gen.anyInt)),
      testM("partialOrd")(checkAllLaws(PartialOrd)(genValidation))
    ),
    suite("ScalaHashCode consistency")(
      testM("ZValidation")(scalaHashCodeConsistency(genValidation))
    )
  )
}
