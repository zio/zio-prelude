package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.laws._

object NonEmptyListSpec extends DefaultRunnableSpec {

  val genCons: Gen[Random with Sized, ::[Int]] =
    Gen.listOf1(Gen.anyInt)

  val genNonEmptyList: Gen[Random with Sized, NonEmptyList[Int]] =
    genCons.map(NonEmptyList.fromCons)

  def spec = suite("NonEmptyListSpec")(
    suite("laws")(
      // testM("associative")(checkAllLaws(Associative)(genNonEmptyList)),
      testM("equal")(checkAllLaws(Equal)(genNonEmptyList)),
      // testM("hash")(checkAllLaws(Hash)(genNonEmptyList)),
      testM("ord")(checkAllLaws(Ord)(genNonEmptyList))
    ),
    suite("methods")(
      testM("++") {
        check(Gen.listOf1(Gen.anyInt), Gen.listOf1(Gen.anyInt)) { (ls, rs) =>
          val actual   = (NonEmptyList.fromCons(ls) ++ NonEmptyList.fromCons(rs)).toCons
          val expected = ls ::: rs
          actual <-> expected
        }
      },
      testM("corresponds") {
        check(Gen.listOf1(Gen.anyInt), Gen.listOf1(Gen.anyInt), Gen.function2(Gen.boolean)) { (ls, rs, f) =>
          val actual   = NonEmptyList.fromCons(ls).corresponds(NonEmptyList.fromCons(rs))(f)
          val expected = ls.corresponds(rs)(f)
          actual <-> expected
        }
      },
      testM("flatMap") {
        check(Gen.listOf1(Gen.anyInt), Gen.function(Gen.listOf1(Gen.anyInt))) { (as, f) =>
          val actual   = NonEmptyList.fromCons(as).flatMap(a => NonEmptyList.fromCons(f(a))).toCons
          val expected = as.flatMap(f)
          actual <-> expected
        }
      },
      testM("foldLeft") {
        check(Gen.listOf1(Gen.anyInt), Gen.anyInt, Gen.function2(Gen.anyInt)) { (as, s, f) =>
          NonEmptyList.fromCons(as).foldLeft(s)(f) <-> as.foldLeft(s)(f)
        }
      },
      testM("foldRight") {
        check(Gen.listOf1(Gen.anyInt), Gen.anyInt, Gen.function2(Gen.anyInt)) { (as, s, f) =>
          NonEmptyList.fromCons(as).foldRight(s)(f) <-> as.foldRight(s)(f)
        }
      },
      testM("iterator") {
        check(Gen.listOf1(Gen.anyInt)) { as =>
          NonEmptyList.fromCons(as).iterator.toList <-> as
        }
      },
      testM("length") {
        check(Gen.listOf1(Gen.anyInt)) { as =>
          NonEmptyList.fromCons(as).length <-> as.length
        }
      },
      testM("map") {
        check(Gen.listOf1(Gen.anyInt), Gen.function(Gen.anyInt)) { (as, f) =>
          NonEmptyList.fromCons(as).map(f).toCons <-> as.map(f)
        }
      },
      testM("mkString") {
        check(Gen.listOf1(Gen.anyInt), Gen.anyString, Gen.anyString, Gen.anyString) { (as, start, sep, end) =>
          NonEmptyList.fromCons(as).mkString(start, sep, end) <-> as.mkString(start, sep, end)
        }
      },
      testM("product") {
        check(Gen.listOf1(Gen.anyInt)) { as =>
          NonEmptyList.fromCons(as).product <-> as.product
        }
      },
      testM("reverse") {
        check(Gen.listOf1(Gen.anyInt)) { as =>
          NonEmptyList.fromCons(as).reverse.toCons <-> as.reverse
        }
      },
      testM("sum") {
        check(Gen.listOf1(Gen.anyInt)) { as =>
          NonEmptyList.fromCons(as).sum <-> as.sum
        }
      },
      testM("tails") {
        check(Gen.listOf1(Gen.anyInt)) { as =>
          NonEmptyList.fromCons(as).tails0.map(_.toCons).toCons <-> as.tails.toList.init
        }
      },
      testM("toCons") {
        check(genNonEmptyList) { as =>
          val cons         = as.toCons
          val nonEmptyList = NonEmptyList.fromCons(cons)
          assert(nonEmptyList)(equalTo(as))
        }
      },
      testM("toString") {
        check(Gen.listOf1(Gen.anyInt)) { as =>
          NonEmptyList.fromCons(as).toString <-> as.mkString("NonEmptyList(", ", ", ")")
        }
      }
    ),
    suite("constructors")(
      testM("fromCons") {
        check(genCons) { as =>
          val nonEmptyList = NonEmptyList.fromCons(as)
          val cons         = nonEmptyList.toCons
          assert(cons)(equalTo(as))
        }
      }
    )
  )
}
