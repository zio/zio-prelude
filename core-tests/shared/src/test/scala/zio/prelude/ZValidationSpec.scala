package zio.prelude

import zio.prelude.Equal._
import zio.prelude.HashSpec.scalaHashCodeConsistency
import zio.prelude.ZValidation._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._
import zio.{Has, Random}

object ZValidationSpec extends DefaultRunnableSpec {

  val genValidation: Gen[Has[Random] with Has[Sized], ZValidation[Int, Int, Int]] =
    Gens.validation(Gen.anyInt, Gen.anyInt, Gen.anyInt)

  val genFValidation: GenF[Has[Random] with Has[Sized], ({ type lambda[+x] = ZValidation[Int, Int, x] })#lambda] =
    GenFs.validation(Gen.anyInt, Gen.anyInt)

  val genFValidationFailure
    : GenF[Has[Random] with Has[Sized], ({ type lambda[+x] = newtypes.Failure[ZValidation[Int, x, Int]] })#lambda] =
    GenFs.validationFailure(Gen.anyInt, Gen.anyInt)

  def spec: ZSpec[Environment, Failure] = suite("ZValidationSpec")(
    suite("laws")(
      test("associativeBoth")(checkAllLaws(AssociativeBothLaws)(genFValidation, Gen.anyInt)),
      test("commutativeBoth")(checkAllLaws(CommutativeBothLaws)(genFValidation, Gen.anyInt)),
      test("covariant")(checkAllLaws(CovariantLaws)(genFValidation, Gen.anyInt)),
      test("equal")(checkAllLaws(EqualLaws)(genValidation)),
      test("hash")(checkAllLaws(HashLaws)(genValidation)),
      test("identityBoth")(checkAllLaws(IdentityBothLaws)(genFValidation, Gen.anyInt)),
      test("partialOrd")(checkAllLaws(PartialOrdLaws)(genValidation))
    ),
    suite("ScalaHashCode consistency")(
      test("ZValidation")(scalaHashCodeConsistency(genValidation))
    ),
    suite("combinators")(
      suite("orElse")(
        test("If first Validation fails use the second") {
          val first: ZValidation[String, String, Nothing] = ZValidation.fail("fail").log("one")
          val second: ZValidation[String, Nothing, Int]   = ZValidation.succeed(1).log("two")

          // NOTE: Requires Unit to find Equal instance, see #113
          val result: ZValidation[String, Unit, Int] = first orElse second

          val expected: ZValidation[String, Nothing, Int] =
            ZValidation
              .succeed(1)
              .log("one")
              .log("two")

          assert(result)(equalTo(expected))
          assert(result.getLog)(equalTo(expected.getLog))
        }
      ),
      suite(label = "orElseLog")(
        test("Transfer Error to Log if its of the same type") {
          val first: ZValidation[String, String, Nothing] = ZValidation.fail("fail").log("one")
          val second: ZValidation[String, Nothing, Int]   = ZValidation.succeed(1).log("two")

          // NOTE: Requires Unit to find Equal instance, see #113
          val result: ZValidation[String, Unit, Int] = first orElseLog second

          val expected: ZValidation[String, Nothing, Int] =
            ZValidation
              .succeed(1)
              .log("one")
              .log("fail")
              .log("two")

          assert(result)(equalTo(expected))
          assert(result.getLog)(equalTo(expected.getLog))
        }
      )
    )
  )
}
