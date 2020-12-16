package zio.prelude

import zio.test._
import zio.test.laws._

object PartialOrdSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("OrdSpec")(
      suite("laws")(
        testM("unit")(checkAllLaws(PartialOrd)(Gen.unit)),
        testM("boolean")(checkAllLaws(PartialOrd)(Gen.boolean)),
        testM("byte")(checkAllLaws(PartialOrd)(Gen.anyByte)),
        testM("char")(checkAllLaws(PartialOrd)(Gen.anyChar)),
        testM("string")(checkAllLaws(PartialOrd)(Gen.anyString)),
        testM("int")(checkAllLaws(PartialOrd)(Gen.anyInt)),
        testM("long")(checkAllLaws(PartialOrd)(Gen.anyLong)),
        testM("float")(checkAllLaws(PartialOrd)(Gen.anyFloat)),
        testM("double")(checkAllLaws(PartialOrd)(Gen.anyDouble)),
        testM("option")(checkAllLaws(PartialOrd)(Gen.option(Gen.anyInt))),
        testM("either")(checkAllLaws(PartialOrd)(Gen.either(Gen.anyInt, Gen.anyInt))),
        testM("tuple2")(checkAllLaws(PartialOrd)(Gen.anyInt.zip(Gen.anyInt))),
        testM("tuple3")(
          checkAllLaws(PartialOrd)(Gen.anyInt.zip(Gen.anyInt).zip(Gen.anyInt).map { case ((x, y), z) => (x, y, z) })
        ),
        testM("set")(checkAllLaws(PartialOrd)(Gen.setOf(Gen.anyInt))),
        testM("list")(checkAllLaws(PartialOrd)(Gen.listOf(Gen.anyInt))),
        testM("vector")(checkAllLaws(PartialOrd)(Gen.vectorOf(Gen.anyInt))),
        testM("chunk")(checkAllLaws(PartialOrd)(Gen.chunkOf(Gen.anyInt))),
        testM("map")(checkAllLaws(PartialOrd)(Gen.mapOf(Gen.anyInt, Gen.anyInt)))
      ),
      test("map ord") {
        assert(Map.empty[Int, Int] =??= Map.empty[Int, Int])(equalTo(Ordering.Equals)) &&
        assert(Map(1 -> 2) =??= Map.empty[Int, Int])(equalTo(Ordering.GreaterThan)) &&
        assert(Map.empty[Int, Int] =??= Map(1 -> 2))(equalTo(Ordering.LessThan)) &&
        assert(Map(1 -> 2) =??= Map(1 -> 2))(equalTo(Ordering.Equals)) &&
        assert(Map(1 -> 2) =??= Map(1 -> 3))(equalTo(Ordering.LessThan)) &&
        assert(Map(1 -> 3) =??= Map(1 -> 2))(equalTo(Ordering.GreaterThan)) &&
        assert(Map(1 -> 2) =??= Map(1 -> 3, 2 -> 4))(equalTo(Ordering.LessThan)) &&
        assert(Map(1 -> 3, 2 -> 4) =??= Map(1 -> 2))(equalTo(Ordering.GreaterThan)) &&
        assert(Map(1 -> 2) =??= Map(2 -> 2))(equalTo(PartialOrdering.Incomparable)) &&
        assert(Map(1 -> 2, 3 -> 3) =??= Map(3 -> 3, 2 -> 2))(equalTo(PartialOrdering.Incomparable)) &&
        assert(Map(1 -> 2, 3 -> 3) =??= Map(3 -> 1, 2 -> 2))(equalTo(PartialOrdering.Incomparable))
      }
    )
}
