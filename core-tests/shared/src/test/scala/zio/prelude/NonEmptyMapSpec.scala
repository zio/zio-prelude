package zio.prelude

import zio.Random
import zio.test._

object NonEmptyMapSpec extends DefaultRunnableSpec {

  private lazy val genMap: Gen[Random with Sized, Map[Int, Int]] =
    Gen.mapOf1(genInt, genInt)

  private lazy val genInt: Gen[Random, Int] =
    Gen.int(-10, 10)

  def spec: ZSpec[Environment, Failure] =
    suite("NonEmptyMapSpec")(
      suite("hashMap methods")(
        test("++") {
          check(genMap, genMap) { (as, bs) =>
            val actual   = (NonEmptyMap.fromMapOption(as).get ++ NonEmptyMap.fromMapOption(bs).get).toMap
            val expected = as ++ bs
            assertTrue(actual == expected)
          }
        },
        test("--") {
          check(genMap, genMap) { (as, bs) =>
            val actual   = (NonEmptyMap.fromMapOption(as).get -- NonEmptyMap.fromMapOption(bs).get.toMap.keys)
            val expected = as -- bs.keys
            assertTrue(actual == expected)
          }
        },
        test("contains") {
          check(genMap, genInt) { (as, a) =>
            assertTrue(NonEmptyMap.fromMapOption(as).get.contains(a) == as.contains(a))
          }
        },
        test("+") {
          check(genMap, genInt, genInt) { (as, k, v) =>
            val actual   = (NonEmptyMap.fromMapOption(as).get + (k, v)).toMap
            val expected = as + (k -> v)
            assertTrue(actual == expected)
          }
        },
        test("-") {
          check(genMap, genInt) { (as, a) =>
            val actual   = NonEmptyMap.fromMapOption(as).get - a
            val expected = as - a
            assertTrue(actual == expected)
          }
        },
        test("size") {
          check(genMap) { as =>
            val actual   = NonEmptyMap.fromMapOption(as).get.size
            val expected = as.size
            assertTrue(actual == expected)
          }
        },
      suite("constructors")(
        test("fromMapOption") {
          check(genMap) { as =>
            val nonEmptyMap = NonEmptyMap.fromMapOption(as).get
            val map         = nonEmptyMap.toMap
            assertTrue(map == as)
          }
        }
      ),
    )
  )
}
