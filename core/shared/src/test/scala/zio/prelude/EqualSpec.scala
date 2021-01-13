package zio.prelude

import zio.test.Gen.oneOf
import zio.test._
import zio.test.laws._

object EqualSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("EqualSpec")(
      suite("laws")(
        {
          implicit val ThrowableEqual: Equal[Throwable] = Equal.ThrowableHash
          testM("throwable")(checkAllLaws(Equal)(Gen.throwable))
        },
        testM("try")(
          checkAllLaws(Equal)(oneOf(Gen.throwable.map(scala.util.Failure(_)), Gen.anyInt.map(scala.util.Success(_))))
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
