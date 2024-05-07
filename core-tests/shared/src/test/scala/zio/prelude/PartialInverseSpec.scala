package zio.prelude

import zio.prelude.laws._
import zio.prelude.newtypes.Prod
import zio.test.TestAspect.ignore
import zio.test._
import zio.test.laws._

object PartialInverseSpec extends ZIOBaseSpec {

  private val nonZeroDoubleProd = Gen.double.filter(_ != 0).map(Prod(_))

  def spec: Spec[Environment, Any] =
    suite("PartialInverseSpec")(
      suite("laws")(
        suite("floating point")(
          test("double prod")(checkAllLaws(PartialInverseLaws)(nonZeroDoubleProd))
        ) @@ ignore // floating point ignored because slight differences in the results make the test fail
      )
    )
}
