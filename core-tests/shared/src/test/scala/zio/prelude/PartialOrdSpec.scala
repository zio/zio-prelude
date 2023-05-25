package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object PartialOrdSpec extends ZIOBaseSpec {

  def spec: Spec[Environment, Any] =
    suite("OrdSpec")(
      suite("laws")(
        test("map")(checkAllLaws(PartialOrdLaws)(Gen.mapOf(Gen.int, Gen.int))),
        test("set")(checkAllLaws(PartialOrdLaws)(Gen.setOf(Gen.int)))
      ),
      test("map compareSoft") {
        assert(PartialOrd.compareSoft(Map.empty[Int, Int], Map.empty[Int, Int]))(equalTo(Ordering.Equals)) &&
        assert(PartialOrd.compareSoft(Map(1 -> 2), Map.empty[Int, Int]))(equalTo(Ordering.GreaterThan)) &&
        assert(PartialOrd.compareSoft(Map.empty[Int, Int], Map(1 -> 2)))(equalTo(Ordering.LessThan)) &&
        assert(PartialOrd.compareSoft(Map(1 -> 2), Map(1 -> 2)))(equalTo(Ordering.Equals)) &&
        assert(PartialOrd.compareSoft(Map(1 -> 2), Map(1 -> 3)))(equalTo(Ordering.LessThan)) &&
        assert(PartialOrd.compareSoft(Map(1 -> 3), Map(1 -> 2)))(equalTo(Ordering.GreaterThan)) &&
        assert(PartialOrd.compareSoft(Map(1 -> 2), Map(1 -> 3, 2 -> 4)))(equalTo(Ordering.LessThan)) &&
        assert(PartialOrd.compareSoft(Map(1 -> 3, 2 -> 4), Map(1 -> 2)))(equalTo(Ordering.GreaterThan)) &&
        assert(PartialOrd.compareSoft(Map(1 -> 2), Map(2 -> 2)))(equalTo(PartialOrdering.Incomparable)) &&
        assert(PartialOrd.compareSoft(Map(1 -> 2, 3 -> 3), Map(3 -> 3, 2 -> 2)))(
          equalTo(PartialOrdering.Incomparable)
        ) &&
        assert(PartialOrd.compareSoft(Map(1 -> 2, 3 -> 3), Map(3 -> 1, 2 -> 2)))(
          equalTo(PartialOrdering.Incomparable)
        ) &&
        assert(PartialOrd.compareSoft(Map("William" -> "Will"), Map("William" -> "Bill")))(
          equalTo(Ordering.GreaterThan)
        )
      }
    )
}
