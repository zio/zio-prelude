package zio.prelude.experimental

import zio.prelude.Equal
import zio.test._
import zio.test.laws._

object DistributiveMultiplySpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("DistributiveMultiplySpec")(
      suite("laws")(
        testM("double distributive multiply")(checkAllLaws(DistributiveMultiply)(Gen.anyDouble)),
        testM("int distributive multiply")(checkAllLaws(DistributiveMultiply)(Gen.anyInt))
      )
    )
}
