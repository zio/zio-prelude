package zio.prelude.experimental

import zio.prelude.experimental.laws._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object DistributiveProdSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveProdSpec")(
      suite("laws")(
        testM("Cause distributive multiply")(
          checkAllLaws(DistributiveProdLaws)(Gen.causes(Gen.anyInt, Gen.throwable))
        ),
        testM("ParSeq distributive multiply")(checkAllLaws(DistributiveProdLaws)(Gens.parSeq(Gen.unit, Gen.anyInt)))
      )
    )
}
