package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object ForEachFilterSpec extends ZIOBaseSpec {

  val genBoolean: Gen[Any, Boolean] =
    Gen.boolean

  val genInt: Gen[Any, Int] =
    Gen.int

  val genList: Gen[Sized, List[Int]] =
    Gen.listOf(genInt)

  val genOptionList: Gen[Sized, List[Option[Int]]] =
    Gen.listOf(Gen.option(genInt))

  val genIntOptionBooleanFunction: Gen[Any, Int => Option[Boolean]] =
    Gen.function(Gen.option(genBoolean))

  def spec: Spec[Environment, Any] =
    suite("ForEachFilterSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(ForEachFilterLaws)(GenF.chunk, Gen.int)),
        test("list")(checkAllLaws(ForEachFilterLaws)(GenF.list, Gen.int)),
        test("map")(checkAllLaws(ForEachFilterLaws)(GenFs.map(Gen.int), Gen.int)),
        test("option")(checkAllLaws(ForEachFilterLaws)(GenF.option, Gen.int)),
        test("vector")(checkAllLaws(ForEachFilterLaws)(GenF.vector, Gen.int))
      ),
      suite("combinators")(
        test("filterA") {
          check(genList, genIntOptionBooleanFunction) { (as, f) =>
            val actual   = ForEachFilter[List].filterA(as)(f)
            val expected = as.foldRight(Option(List.empty[Int])) { (a, acc) =>
              acc.flatMap(list => f(a).map(if (_) a :: list else list))
            }
            assert(actual)(equalTo(expected))
          }
        },
        test("mapFilter") {
          check(genList, genIntOptionBooleanFunction) { (as, f) =>
            val actual   = ForEachFilter[List].mapFilter(as)(f)
            val expected = as.flatMap(f(_))
            assert(actual)(equalTo(expected))
          }
        },
        test("collect") {
          check(genOptionList) { as =>
            val actual   = ForEachFilter[List].collect(as) { case Some(a) => a }
            val expected = as.flatten
            assert(actual)(equalTo(expected))
          }
        }
      )
    )
}
