package zio.prelude

import zio.prelude.Equal._
import zio.prelude.Validation._
import zio.prelude.coherent._
import zio.random.Random
import zio.test._
import zio.test.laws._

object ValidationSpec extends DefaultRunnableSpec {

  val genValidation: Gen[Random with Sized, Validation[Int, Int]] =
    Gens.validation(Gen.anyInt, Gen.anyInt)

  val genFValidation: GenF[Random with Sized, ({ type lambda[+x] = Validation[Int, x] })#lambda] =
    GenFs.validation(Gen.anyInt)

  val genFValidationFailure
    : GenF[Random with Sized, ({ type lambda[+x] = newtypes.Failure[Validation[x, Int]] })#lambda] =
    GenFs.validationFailure(Gen.anyInt)

  def spec: ZSpec[Environment, Failure] =
    suite("ValidationSpec")(
      suite("laws")(
        testM("associativeBoth")(checkAllLaws(AssociativeBoth)(genFValidation, Gen.anyInt)),
        testM("commutativeBoth")(checkAllLaws(CommutativeBoth)(genFValidation, Gen.anyInt)),
        testM("covariant")(checkAllLaws(Covariant)(genFValidation, Gen.anyInt)),
        testM("equal")(checkAllLaws(Equal)(genValidation)),
        testM("failureCovariant")(
          checkAllLaws[
            CovariantDeriveEqual,
            Equal,
            Any,
            Random with Sized,
            ({ type lambda[+x] = newtypes.Failure[Validation[x, Int]] })#lambda,
            Int
          ](Covariant)(genFValidationFailure, Gen.anyInt)(
            // Scala 2.11 doesn't seem to be able to infer the type parameter for CovariantDeriveEqual.derive
            CovariantDeriveEqual.derive[({ type lambda[+x] = newtypes.Failure[Validation[x, Int]] })#lambda](
              ValidationFailureCovariant,
              ValidationFailureDeriveEqual(IntHashOrd)
            ),
            IntHashOrd
          )
        ),
        testM("hash")(checkAllLaws(Hash)(genValidation)),
        testM("identityBoth")(checkAllLaws(IdentityBoth)(genFValidation, Gen.anyInt)),
        testM("ord")(checkAllLaws(Ord)(genValidation))
      )
    )
}
