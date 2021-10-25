package zio.prelude.experimental

import zio.prelude._
import zio.prelude.experimental.laws._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object DistributiveMultiplySpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveMultiplySpec")(
      suite("laws")(
        testM("double distributive multiply")(checkAllLaws(DistributiveMultiplyLaws)(Gen.anyDouble)),
        testM("int distributive multiply")(checkAllLaws(DistributiveMultiplyLaws)(Gen.anyInt)),
//        depends on https://github.com/zio/zio/pull/5242
//        testM("Cause distributive multiply")(checkAllLaws(DistributiveMultiply)(Gen.causes(Gen.anyInt, Gen.throwable))),
        testM("ParSeq distributive multiply")(checkAllLaws(DistributiveMultiplyLaws)(Gens.parSeq(Gen.unit, Gen.anyInt)))
      )
    )
}
