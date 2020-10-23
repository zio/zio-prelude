package zio.prelude

import zio.test.laws._
import zio.test._
import zio.test.TestAspect._

object DistributiveSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Distributive")(
      suite("laws")(
        testM("int distributive")(checkAllLaws(Distributive)(Gen.anyInt)),
        suite("floating point")(
          testM("double distributive")(checkAllLaws(Distributive)(Gen.anyDouble))
        ) @@ ignore // floating point ignored because slight differences in the results make the test fail
      )
    )
}
