package zio.prelude

import zio.prelude.laws.InverseLaws
import zio.prelude.newtypes.{OrF, Sum}
import zio.test.laws._
import zio.test.{DefaultRunnableSpec, _}

object InverseSpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("InverseSpec")(
      suite("laws")(
        testM("byte addition")(checkAllLaws(InverseLaws)(Gen.anyByte.map(Sum(_)))),
        testM("char addition")(checkAllLaws(InverseLaws)(Gen.anyChar.map(Sum(_)))),
        testM("double addition")(checkAllLaws(InverseLaws)(Gen.anyDouble.map(Sum(_)))),
        testM("float addition")(checkAllLaws(InverseLaws)(Gen.anyFloat.map(Sum(_)))),
        testM("int addition")(checkAllLaws(InverseLaws)(Gen.anyInt.map(Sum(_)))),
        testM("long addition")(checkAllLaws(InverseLaws)(Gen.anyLong.map(Sum(_)))),
        testM("set")(checkAllLaws(InverseLaws)(Gen.setOf(Gen.anyInt).map(OrF(_)))),
        testM("short addition")(checkAllLaws(InverseLaws)(Gen.anyShort.map(Sum(_)))),
        testM("tuple2")(checkAllLaws(InverseLaws)(Gen.anyInt.map(Sum(_)).zip(Gen.anyInt.map(Sum(_))))),
        testM("tuple3")(
          checkAllLaws(InverseLaws)(Gen.anyInt.map(Sum(_)).zip(Gen.anyInt.map(Sum(_))).zip(Gen.anyInt.map(Sum(_))).map {
            case ((a, b), c) => (a, b, c)
          })
        )
      )
    )
}
