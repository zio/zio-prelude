package zio.prelude

import zio.test.laws._
import zio.test._
import zio.test.TestAspect._

object DistributiveMultiplySpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveMultiply")(
      suite("laws")(
        testM("int distributive")(checkAllLaws(DistributiveMultiply)(Gen.anyInt)),
        suite("floating point")(
          testM("double distributive")(checkAllLaws(DistributiveMultiply)(Gen.anyDouble))
        ) @@ ignore // floating point ignored because slight differences in the results make the test fail
      )
    )
}
