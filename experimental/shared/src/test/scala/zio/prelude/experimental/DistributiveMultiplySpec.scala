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
//        TODO: should Cause satisfy distributivity???
//        Then(Fail(257),Both(Empty,Empty)) did not satisfy (equalTo(Both(Fail(257),Fail(257))) ?? "leftDistributivityLaw")
//        Then(Both(Empty,Empty),Fail(0)) did not satisfy (equalTo(Both(Fail(0),Fail(0))) ?? "rightDistributivityLaw")
//        testM("Cause distributive multiply")(checkAllLaws(DistributiveMultiply)(Gen.causes(Gen.anyInt, Gen.throwable))),
        testM("ParSeq distributive multiply")(checkAllLaws(DistributiveMultiply)(Gens.parSeq(Gen.unit, Gen.anyInt)))
      )
    )
}
