package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object PartialOrdSpec extends ZIOSpecDefault {

  def spec: ZSpec[Environment, Nothing] =
    suite("OrdSpec")(
      suite("laws")(
        test("map")(checkAllLaws(PartialOrdLaws)(Gen.mapOf(Gen.int, Gen.int))),
        test("set")(checkAllLaws(PartialOrdLaws)(Gen.setOf(Gen.int)))
      ),
      test("map compareSoft") {
        assert(Map.empty[Int, Int] compareSoft Map.empty[Int, Int])(equalTo(Ordering.Equals)) &&
        assert(Map(1 -> 2) compareSoft Map.empty[Int, Int])(equalTo(Ordering.GreaterThan)) &&
        assert(Map.empty[Int, Int] compareSoft Map(1 -> 2))(equalTo(Ordering.LessThan)) &&
        assert(Map(1 -> 2) compareSoft Map(1 -> 2))(equalTo(Ordering.Equals)) &&
        assert(Map(1 -> 2) compareSoft Map(1 -> 3))(equalTo(Ordering.LessThan)) &&
        assert(Map(1 -> 3) compareSoft Map(1 -> 2))(equalTo(Ordering.GreaterThan)) &&
        assert(Map(1 -> 2) compareSoft Map(1 -> 3, 2 -> 4))(equalTo(Ordering.LessThan)) &&
        assert(Map(1 -> 3, 2 -> 4) compareSoft Map(1 -> 2))(equalTo(Ordering.GreaterThan)) &&
        assert(Map(1 -> 2) compareSoft Map(2 -> 2))(equalTo(PartialOrdering.Incomparable)) &&
        assert(Map(1 -> 2, 3 -> 3) compareSoft Map(3 -> 3, 2 -> 2))(equalTo(PartialOrdering.Incomparable)) &&
        assert(Map(1 -> 2, 3 -> 3) compareSoft Map(3 -> 1, 2 -> 2))(equalTo(PartialOrdering.Incomparable)) &&
        assert(Map("William" -> "Will") compareSoft Map("William" -> "Bill"))(equalTo(Ordering.GreaterThan))
      }
    )
}
