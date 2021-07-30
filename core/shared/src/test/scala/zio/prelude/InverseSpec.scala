package zio.prelude

import zio.prelude.newtypes.Sum
import zio.test.laws._
import zio.test.{DefaultRunnableSpec, _}

object InverseSpec extends DefaultRunnableSpec {

  private implicit val DoubleEqual: Equal[Double] = Equal.DoubleEqualWithEpsilon()
  private implicit val FloatEqual: Equal[Float]   = Equal.FloatEqualWithEpsilon()

  def spec: ZSpec[Environment, Failure] =
    suite("InverseSpec")(
      suite("laws")(
        test("byte addition")(checkAllLaws(Inverse)(Gen.anyByte.map(Sum(_)))),
        test("char addition")(checkAllLaws(Inverse)(Gen.anyChar.map(Sum(_)))),
        test("double addition")(checkAllLaws(Inverse)(Gen.anyDouble.map(Sum(_)))),
        test("float addition")(checkAllLaws(Inverse)(Gen.anyFloat.map(Sum(_)))),
        test("int addition")(checkAllLaws(Inverse)(Gen.anyInt.map(Sum(_)))),
        test("long addition")(checkAllLaws(Inverse)(Gen.anyLong.map(Sum(_)))),
        test("set")(checkAllLaws(Inverse)(Gen.setOf(Gen.anyInt))),
        test("short addition")(checkAllLaws(Inverse)(Gen.anyShort.map(Sum(_)))),
        test("tuple2")(checkAllLaws(Inverse)(Gen.anyInt.map(Sum(_)).zip(Gen.anyInt.map(Sum(_))))),
        test("tuple3")(
          checkAllLaws(Inverse)(Gen.anyInt.map(Sum(_)).zip(Gen.anyInt.map(Sum(_))).zip(Gen.anyInt.map(Sum(_))))
        )
      )
    )
}
