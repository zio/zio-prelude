package zio.prelude

import zio.Random
import zio.test._

import scala.collection.immutable.{SortedMap, TreeMap}

object NonEmptySortedMapSpec extends DefaultRunnableSpec {

  private lazy val genMap: Gen[Random with Sized, SortedMap[Int, Int]] =
    Gen.mapOf1(genInt, genInt).map(SortedMap.from(_))

  private lazy val genInt: Gen[Random, Int] =
    Gen.int(-10, 10)

  def spec: ZSpec[Environment, Failure] =
    suite("NonEmptyMapSpec")(
      suite("treemap methods - note we check sort order")(
        test("++") {
          check(genMap, genMap) { (as, bs) =>
            val actual   = (NonEmptySortedMap.fromMapOption(as).get ++ NonEmptySortedMap.fromMapOption(bs).get)
            val expected = TreeMap.from(as ++ bs)
            assertTrue(actual.toVector == expected.toVector)
          }
        },
        test("contains") {
          check(genMap, genInt) { (as, a) =>
            assertTrue(NonEmptySortedMap.fromMapOption(as).get.contains(a) == as.contains(a))
          }
        },
        test("+") {
          check(genMap, genInt, genInt) { (as, a, b) =>
            val actual   = NonEmptySortedMap.fromMapOption(as).get + (a -> b)
            val expected = TreeMap.from(as + (a->b))
            assertTrue(actual.toVector == expected.toVector)
          }
        },
        test("-") {
          check(genMap, genInt) { (as, a) =>
            val actual   = NonEmptySortedMap.fromMapOption(as).get - a
            val expected = TreeMap.from(as - a)
            assertTrue(actual.toVector == expected.toVector)
          }
        },
        test("size") {
          check(genMap) { as =>
            val actual   = NonEmptySortedMap.fromMapOption(as).get.size
            val expected = as.size
            assertTrue(actual == expected)
          }
        },
        suite("constructors")(
          test("fromMapOption") {
            check(genMap) { as =>
              val nonEmptyMap = NonEmptySortedMap.fromMapOption(as).get
              val map         = nonEmptyMap.toMap
              assertTrue(map == as)
            }
          }
        )
      )
    )
}
