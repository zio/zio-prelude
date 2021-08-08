package zio.prelude

import zio.prelude.Common.anyFiniteDurationScala
import zio.prelude.newtypes.Sum
import zio.test.laws._
import zio.test.{DefaultRunnableSpec, _}

object InverseSpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("InverseSpec")(
      suite("laws")(
        testM("byte addition")(checkAllLaws(Inverse)(Gen.anyByte.map(Sum(_)))),
        testM("char addition")(checkAllLaws(Inverse)(Gen.anyChar.map(Sum(_)))),
        testM("double addition")(checkAllLaws(Inverse)(Gen.anyDouble.map(Sum(_)))),
        testM("duration Scala")(checkAllLaws(Inverse)(anyFiniteDurationScala)),
        testM("float addition")(checkAllLaws(Inverse)(Gen.anyFloat.map(Sum(_)))),
        testM("int addition")(checkAllLaws(Inverse)(Gen.anyInt.map(Sum(_)))),
        testM("long addition")(checkAllLaws(Inverse)(Gen.anyLong.map(Sum(_)))),
        testM("set")(checkAllLaws(Inverse)(Gen.setOf(Gen.anyInt))),
        testM("short addition")(checkAllLaws(Inverse)(Gen.anyShort.map(Sum(_)))),
        testM("tuple2")(checkAllLaws(Inverse)(Gen.anyInt.map(Sum(_)).zip(Gen.anyInt.map(Sum(_))))),
        testM("tuple3")(
          checkAllLaws(Inverse)(Gen.anyInt.map(Sum(_)).zip(Gen.anyInt.map(Sum(_))).zip(Gen.anyInt.map(Sum(_))).map {
            case ((a, b), c) => (a, b, c)
          })
        )
      )
    )
}
