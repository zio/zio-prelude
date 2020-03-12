package zio.prelude

import zio.test._
import zio.test.laws._

object EqualSpec extends DefaultRunnableSpec {

  def spec = suite("EqualSpec")(
    suite("laws")(
      testM("boolean")(checkAllLaws(Equal)(Gen.boolean)),
      testM("byte")(checkAllLaws(Equal)(Gen.anyByte)),
      testM("char")(checkAllLaws(Equal)(Gen.anyChar)),
      testM("double")(checkAllLaws(Equal)(Gen.anyDouble)),
      testM("either")(checkAllLaws(Equal)(Gen.either(Gen.anyInt, Gen.anyInt))),
      testM("float")(checkAllLaws(Equal)(Gen.anyFloat)),
      testM("int")(checkAllLaws(Equal)(Gen.anyInt)),
      testM("list")(checkAllLaws(Equal)(Gen.listOf(Gen.anyInt))),
      testM("long")(checkAllLaws(Equal)(Gen.anyLong)),
      testM("map")(checkAllLaws(Equal)(TestUtil.anyMap(Gen.anyInt, Gen.anyInt))),
      testM("option")(checkAllLaws(Equal)(Gen.option(Gen.anyInt))),
      testM("set")(checkAllLaws(Equal)(TestUtil.anySet(Gen.anyInt))),
      testM("string")(checkAllLaws(Equal)(Gen.anyString)),
      testM("tuple")(checkAllLaws(Equal)(Gen.anyInt.zip(Gen.anyInt))),
      testM("unit")(checkAllLaws(Equal)(Gen.unit)),
      testM("vector")(checkAllLaws(Equal)(Gen.vectorOf(Gen.anyInt)))
    ),
    test("DoubleEqual correctly handles `Double.NaN") {
      Double.NaN <-> Double.NaN
    },
    test("FloatEqual  correctly handles `Float.NaN") {
      Float.NaN <-> Float.NaN
    }
  )
}
