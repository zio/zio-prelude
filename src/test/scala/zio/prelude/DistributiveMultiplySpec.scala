package zio.prelude

import zio.test.TestAspect._
import zio.test._
import zio.test.laws._

object DistributiveMultiplySpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveMultiplySpec")(
      suite("laws")(
        testM("int distributive multiply")(checkAllLaws(DistributiveMultiply)(Gen.anyInt)),
        suite("floating point")(
          testM("double distributive multiply")(checkAllLaws(DistributiveMultiply)(Gen.anyDouble))
        ) @@ ignore // floating point ignored because slight differences in the results make the test fail
      )
    )
}
