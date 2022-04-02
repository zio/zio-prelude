package zio.prelude

import zio.prelude.laws._
import zio.test.Gen.oneOf
import zio.test._
import zio.test.laws._

object EqualSpec extends ZIOSpecDefault {

  def spec: ZSpec[Environment, Failure] =
    suite("EqualSpec")(
      suite("laws")(
        {
          implicit val ThrowableEqual: Equal[Throwable] = Equal.ThrowableHash
          test("throwable")(checkAllLaws(EqualLaws)(Gen.throwable))
        },
        test("try")(
          checkAllLaws(EqualLaws)(
            oneOf(Gen.throwable.map(scala.util.Failure(_)), Gen.int.map(scala.util.Success(_)))
          )
        )
      ),
      test("DoubleEqual correctly handles `Double.NaN") {
        Double.NaN <-> Double.NaN
      },
      test("FloatEqual  correctly handles `Float.NaN") {
        Float.NaN <-> Float.NaN
      }
    )
}
