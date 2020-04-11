package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.laws._

object NonEmptyListSpec extends DefaultRunnableSpec {

  lazy val genBooleanFunction: Gen[Random, Int => Boolean] =
    Gen.function(Gen.boolean)

  lazy val genBooleanFunction2: Gen[Random, (Int, Int) => Boolean] =
    Gen.function2(Gen.boolean)

  lazy val genCons: Gen[Random with Sized, ::[Int]] =
    Gen.listOf1(genInt)

  lazy val genConsFunction: Gen[Random with Sized, Int => ::[Int]] =
    Gen.function(genCons)

  lazy val genFunction: Gen[Random, Int => Int] =
    Gen.function(genInt)

  lazy val genFunction2: Gen[Random with Sized, (Int, Int) => Int] =
    Gen.function2(genInt)

  lazy val genInt: Gen[Random, Int] =
    Gen.int(-10, 10)

  lazy val genNonEmptyList: Gen[Random with Sized, NonEmptyList[Int]] =
    genCons.map(NonEmptyList.fromCons)

  lazy val genString: Gen[Random with Sized, String] =
    Gen.alphaNumericString

  def spec = suite("NonEmptyListSpec")(
    suite("laws")(
      testM("associative")(checkAllLaws(Associative)(genNonEmptyList)),
      testM("equal")(checkAllLaws(Equal)(genNonEmptyList)),
      testM("hash")(checkAllLaws(Hash)(genNonEmptyList)),
      testM("ord")(checkAllLaws(Ord)(genNonEmptyList))
    ),
    suite("methods")(
      testM("++") {
        check(genCons, genCons) { (as, bs) =>
          val actual   = (NonEmptyList.fromCons(as) ++ NonEmptyList.fromCons(bs)).toCons
          val expected = as ::: bs
          actual <-> expected
        }
      },
      testM("contains") {
        check(genCons, genInt) { (as, a) =>
          NonEmptyList.fromCons(as).contains(a) <-> as.contains(a)
        }
      },
      testM("corresponds") {
        check(genCons, genCons, genBooleanFunction2) { (as, bs, f) =>
          val actual   = NonEmptyList.fromCons(as).corresponds(NonEmptyList.fromCons(bs))(f)
          val expected = as.corresponds(bs)(f)
          actual <-> expected
        }
      },
      testM("count") {
        check(genCons, genBooleanFunction) { (as, f) =>
          NonEmptyList.fromCons(as).count(f) <-> as.count(f)
        }
      },
      testM("exists") {
        check(genCons, genBooleanFunction) { (as, f) =>
          NonEmptyList.fromCons(as).exists(f) <-> as.exists(f)
        }
      },
      testM("find") {
        check(genCons, genBooleanFunction) { (as, f) =>
          NonEmptyList.fromCons(as).find(f) <-> as.find(f)
        }
      },
      testM("flatMap") {
        check(genCons, genConsFunction) { (as, f) =>
          val actual   = NonEmptyList.fromCons(as).flatMap(a => NonEmptyList.fromCons(f(a))).toCons
          val expected = as.flatMap(f)
          actual <-> expected
        }
      },
      testM("flatten") {
        check(genCons, genConsFunction) { (as, f) =>
          val actual   = NonEmptyList.fromCons(as).map(a => NonEmptyList.fromCons(f(a))).flatten.toCons
          val expected = as.map(f).flatten
          actual <-> expected
        }
      },
      testM("foldLeft") {
        check(genCons, genInt, genFunction2) { (as, z, f) =>
          NonEmptyList.fromCons(as).foldLeft(z)(f) <-> as.foldLeft(z)(f)
        }
      },
      testM("foldRight") {
        check(genCons, genInt, genFunction2) { (as, z, f) =>
          NonEmptyList.fromCons(as).foldRight(z)(f) <-> as.foldRight(z)(f)
        }
      },
      testM("forall") {
        check(genCons, genBooleanFunction) { (as, f) =>
          NonEmptyList.fromCons(as).forall(f) <-> as.forall(f)
        }
      },
      testM("head") {
        check(genCons) { as =>
          NonEmptyList.fromCons(as).head <-> as.head
        }
      },
      testM("length") {
        check(genCons) { as =>
          NonEmptyList.fromCons(as).length <-> as.length
        }
      },
      testM("map") {
        check(genCons, genFunction) { (as, f) =>
          NonEmptyList.fromCons(as).map(f).toCons <-> as.map(f)
        }
      },
      testM("max") {
        check(genCons) { as =>
          NonEmptyList.fromCons(as).max <-> as.max
        }
      },
      testM("maxBy") {
        check(genCons, genFunction) { (as, f) =>
          NonEmptyList.fromCons(as).maxBy(f) <-> as.maxBy(f)
        }
      },
      testM("min") {
        check(genCons) { as =>
          NonEmptyList.fromCons(as).min <-> as.min
        }
      },
      testM("minBy") {
        check(genCons, genFunction) { (as, f) =>
          NonEmptyList.fromCons(as).minBy(f) <-> as.minBy(f)
        }
      },
      testM("mkString") {
        check(genCons, genString, genString, genString) { (as, start, sep, end) =>
          NonEmptyList.fromCons(as).mkString(start, sep, end) <-> as.mkString(start, sep, end)
        }
      },
      testM("product") {
        check(genCons) { as =>
          NonEmptyList.fromCons(as).product <-> as.product
        }
      },
      testM("reduceLeft") {
        check(genCons, genFunction2) { (as, f) =>
          NonEmptyList.fromCons(as).reduceLeft(f) <-> as.reduceLeft(f)
        }
      },
      testM("reduceRight") {
        check(genCons, genFunction2) { (as, f) =>
          NonEmptyList.fromCons(as).reduceRight(f) <-> as.reduceRight(f)
        }
      },
      testM("reverse") {
        check(genCons) { as =>
          NonEmptyList.fromCons(as).reverse.toCons <-> as.reverse
        }
      },
      testM("sum") {
        check(genCons) { as =>
          NonEmptyList.fromCons(as).sum <-> as.sum
        }
      },
      testM("tails") {
        check(genCons) { as =>
          NonEmptyList.fromCons(as).tails.map(_.toCons).toCons <-> as.tails.toList.init
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
        check(genCons) { as =>
          NonEmptyList.fromCons(as).toString <-> as.mkString("NonEmptyList(", ", ", ")")
        }
      },
      testM("zip") {
        check(genCons, genCons) { (as, bs) =>
          val actual   = NonEmptyList.fromCons(as).zip(NonEmptyList.fromCons(bs)).toCons
          val expected = as.zip(bs)
          actual <-> expected
        }
      },
      testM("zipWithIndex") {
        check(genCons) { as =>
          val actual   = NonEmptyList.fromCons(as).zipWithIndex.toCons
          val expected = as.zipWithIndex
          actual <-> expected
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
