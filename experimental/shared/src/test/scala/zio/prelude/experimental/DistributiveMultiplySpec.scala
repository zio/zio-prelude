package zio.prelude.experimental

import zio.prelude.{Equal, Gens}
import zio.test._
import zio.test.laws._

object DistributiveMultiplySpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveMultiplySpec")(
      suite("laws")(
        testM("double distributive multiply")(checkAllLaws(DistributiveMultiply)(Gen.anyDouble)),
        testM("int distributive multiply")(checkAllLaws(DistributiveMultiply)(Gen.anyInt)),
//        depends on https://github.com/zio/zio/pull/5242
//        testM("Cause distributive multiply")(checkAllLaws(DistributiveMultiply)(Gen.causes(Gen.anyInt, Gen.throwable))),
        testM("ParSeq distributive multiply")(checkAllLaws(DistributiveMultiply)(Gens.parSeq(Gen.unit, Gen.anyInt)))
      )
    )
}
