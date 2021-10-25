package zio.prelude.experimental

import zio.prelude.experimental.laws._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object DistributiveMultiplySpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveMultiplySpec")(
      suite("laws")(
        testM("Cause distributive multiply")(
          checkAllLaws(DistributiveMultiplyLaws)(Gen.causes(Gen.anyInt, Gen.throwable))
        ),
        testM("ParSeq distributive multiply")(checkAllLaws(DistributiveMultiplyLaws)(Gens.parSeq(Gen.unit, Gen.anyInt)))
      )
    )
}
