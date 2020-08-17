package zio.prelude

import zio.test.Gen.oneOf
import zio.test._
import zio.test.laws._

object EqualSpec extends DefaultRunnableSpec {

  def spec = suite("EqualSpec")(
    suite("laws")(
      testM("unit")(checkAllLaws(Equal)(Gen.unit)),
      testM("boolean")(checkAllLaws(Equal)(Gen.boolean)),
      testM("byte")(checkAllLaws(Equal)(Gen.anyByte)),
      testM("char")(checkAllLaws(Equal)(Gen.anyChar)),
      testM("string")(checkAllLaws(Equal)(Gen.anyString)),
      testM("int")(checkAllLaws(Equal)(Gen.anyInt)),
      testM("long")(checkAllLaws(Equal)(Gen.anyLong)),
      testM("float")(checkAllLaws(Equal)(Gen.anyFloat)),
      testM("double")(checkAllLaws(Equal)(Gen.anyDouble)),
      testM("option")(checkAllLaws(Equal)(Gen.option(Gen.anyInt))),
      testM("either")(checkAllLaws(Equal)(Gen.either(Gen.anyInt, Gen.anyInt))),
      testM("tuple2")(checkAllLaws(Equal)(Gen.anyInt.zip(Gen.anyInt))),
      testM("tuple3")(checkAllLaws(Equal)(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt))),
      testM("list")(checkAllLaws(Equal)(Gen.listOf(Gen.anyInt))),
      testM("vector")(checkAllLaws(Equal)(Gen.vectorOf(Gen.anyInt))),
      testM("map")(checkAllLaws(Equal)(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
      testM("set")(checkAllLaws(Equal)(Gen.setOf(Gen.anyInt))),
      testM("chunk")(checkAllLaws(Equal)(Gen.chunkOf(Gen.anyInt))),
      testM("throwable")(checkAllLaws(Equal)(Gen.throwable)),
      testM("try")(
        checkAllLaws(Equal)(oneOf(Gen.throwable.map(scala.util.Failure(_)), Gen.anyInt.map(scala.util.Success(_))))
      )
    ),
    test("DoubleEqual correctly handles `Double.NaN") {
      Double.NaN <-> Double.NaN
    },
    test("FloatEqual  correctly handles `Float.NaN") {
      Float.NaN <-> Float.NaN
    }
  )
}
