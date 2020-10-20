package zio.prelude

import zio.test.laws._
import zio.test.{ DefaultRunnableSpec, _ }

object DistributiveSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveSpec")(
      suite("laws")(
        testM("int distributive")(checkAllLaws(Distributive)(Gen.anyInt))
      )
    )
}
