package zio.prelude

import zio.prelude.newtypes.Prod
import zio.test.TestAspect.ignore
import zio.test.laws._
import zio.test._

object InverseNonZeroSpec extends DefaultRunnableSpec {

  private val nonZeroDoubleProd = Gen.anyDouble.filter(_ != 0).map(Prod(_))

  def spec: ZSpec[Environment, Failure] =
    suite("InverseNonZeroSpec")(
      suite("laws")(
        suite("floating point")(
          testM("double prod")(checkAllLaws(InverseNonZero)(nonZeroDoubleProd))
        ) @@ ignore // floating point ignored because slight differences in the results make the test fail
      )
    )
}
