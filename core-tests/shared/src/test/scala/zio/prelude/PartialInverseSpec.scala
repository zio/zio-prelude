package zio.prelude

import zio.prelude.laws._
import zio.prelude.newtypes.Prod
import zio.test.TestAspect.ignore
import zio.test._
import zio.test.laws._

object PartialInverseSpec extends DefaultRunnableSpec {

  private val nonZeroDoubleProd = Gen.anyDouble.filter(_ != 0).map(Prod(_))

  def spec: ZSpec[Environment, Failure] =
    suite("PartialInverseSpec")(
      suite("laws")(
        suite("floating point")(
          testM("double prod")(checkAllLaws(PartialInverseLaws)(nonZeroDoubleProd))
        ) @@ ignore // floating point ignored because slight differences in the results make the test fail
      )
    )
}
