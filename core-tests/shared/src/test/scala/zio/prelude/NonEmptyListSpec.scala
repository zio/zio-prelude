package zio.prelude

import zio.NonEmptyChunk
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object NonEmptyListSpec extends ZIOSpecDefault {

  lazy val genBooleanFunction: Gen[Any, Int => Boolean] =
    Gen.function(Gen.boolean)

  lazy val genBooleanFunction2: Gen[Any, (Int, Int) => Boolean] =
    Gen.function2(Gen.boolean)

  lazy val genCons: Gen[Sized, ::[Int]] =
    Gen.listOf1(genInt)

  lazy val genConsFunction: Gen[Sized, Int => ::[Int]] =
    Gen.function(genCons)

  lazy val genFunction: Gen[Any, Int => Int] =
    Gen.function(genInt)

  lazy val genFunction2: Gen[Sized, (Int, Int) => Int] =
    Gen.function2(genInt)

  lazy val genInt: Gen[Any, Int] =
    Gen.int(-10, 10)

  lazy val genNonEmptyChunk: Gen[Sized, NonEmptyChunk[Int]] =
    Gen.chunkOf1(genInt)

  lazy val genNonEmptyList: Gen[Sized, NonEmptyList[Int]] =
    genCons.map(NonEmptyList.fromCons)

  lazy val genString: Gen[Sized, String] =
    Gen.alphaNumericString

  lazy val genConsWithIndex: Gen[Sized, (::[Int], Int)] =
    for {
      cons  <- genCons
      index <- Gen.int(-2, cons.length + 2)
    } yield (cons, index)

  def spec: Spec[Environment, Any] =
    suite("NonEmptyListSpec")(
      suite("laws")(
        test("associative")(checkAllLaws(AssociativeLaws)(genNonEmptyList)),
        test("associativeEither")(checkAllLaws(AssociativeEitherLaws)(GenFs.nonEmptyList, Gen.int)),
        test("commutativeBoth")(checkAllLaws(CommutativeBothLaws)(GenFs.nonEmptyList, Gen.int)),
        test("hash")(checkAllLaws(HashLaws)(genNonEmptyList)),
        test("identityBoth")(checkAllLaws(IdentityBothLaws)(GenFs.nonEmptyList, Gen.int)),
        test("identityFlatten")(checkAllLaws(IdentityFlattenLaws)(GenFs.nonEmptyList, Gen.int)),
        test("nonEmptyForEach")(checkAllLaws(NonEmptyForEachLaws)(GenFs.nonEmptyList, Gen.int)),
        test("ord")(checkAllLaws(OrdLaws)(genNonEmptyList))
      ),
      suite("methods")(
        test("++") {
          check(genCons, genCons) { (as, bs) =>
            val actual   = (NonEmptyList.fromCons(as) ++ NonEmptyList.fromCons(bs)).toCons
            val expected = as ::: bs
            actual <-> expected
          }
        },
        test("contains") {
          check(genCons, genInt) { (as, a) =>
            NonEmptyList.fromCons(as).contains(a) <-> as.contains(a)
          }
        },
        test("corresponds") {
          check(genCons, genCons, genBooleanFunction2) { (as, bs, f) =>
            val actual   = NonEmptyList.fromCons(as).corresponds(NonEmptyList.fromCons(bs))(f)
            val expected = as.corresponds(bs)(f)
            actual <-> expected
          }
        },
        test("count") {
          check(genCons, genBooleanFunction) { (as, f) =>
            NonEmptyList.fromCons(as).count(f) <-> as.count(f)
          }
        },
        test("distinct") {
          check(genCons) { as =>
            NonEmptyList.fromCons(as).distinct.toCons <-> as.distinct
          }
        },
        test("drop") {
          check(genConsWithIndex) { case (as, i) =>
            NonEmptyList.fromCons(as).drop(i) <-> as.drop(i)
          }
        },
        test("dropRight") {
          check(genConsWithIndex) { case (as, i) =>
            NonEmptyList.fromCons(as).dropRight(i) <-> as.dropRight(i)
          }
        },
        test("dropWhile") {
          check(genCons, genBooleanFunction) { (as, f) =>
            NonEmptyList.fromCons(as).dropWhile(f) <-> as.dropWhile(f)

          }
        },
        test("exists") {
          check(genCons, genBooleanFunction) { (as, f) =>
            NonEmptyList.fromCons(as).exists(f) <-> as.exists(f)
          }
        },
        test("find") {
          check(genCons, genBooleanFunction) { (as, f) =>
            NonEmptyList.fromCons(as).find(f) <-> as.find(f)
          }
        },
        test("flatMap") {
          check(genCons, genConsFunction) { (as, f) =>
            val actual   = NonEmptyList.fromCons(as).flatMap(a => NonEmptyList.fromCons(f(a))).toCons
            val expected = as.flatMap(f)
            actual <-> expected
          }
        },
        test("flatten") {
          check(genCons, genConsFunction) { (as, f) =>
            val actual   = NonEmptyList.fromCons(as).map(a => NonEmptyList.fromCons(f(a))).flatten.toCons
            val expected = as.map(f).flatten
            actual <-> expected
          }
        },
        test("foldLeft") {
          check(genCons, genInt, genFunction2) { (as, z, f) =>
            NonEmptyList.fromCons(as).foldLeft(z)(f) <-> as.foldLeft(z)(f)
          }
        },
        test("foldRight") {
          check(genCons, genInt, genFunction2) { (as, z, f) =>
            NonEmptyList.fromCons(as).foldRight(z)(f) <-> as.foldRight(z)(f)
          }
        },
        test("forall") {
          check(genCons, genBooleanFunction) { (as, f) =>
            NonEmptyList.fromCons(as).forall(f) <-> as.forall(f)
          }
        },
        test("head") {
          check(genCons) { as =>
            NonEmptyList.fromCons(as).head <-> as.head
          }
        },
        test("length") {
          check(genCons) { as =>
            NonEmptyList.fromCons(as).length <-> as.length
          }
        },
        test("map") {
          check(genCons, genFunction) { (as, f) =>
            NonEmptyList.fromCons(as).map(f).toCons <-> as.map(f)
          }
        },
        test("max") {
          check(genCons) { as =>
            NonEmptyList.fromCons(as).max <-> as.max
          }
        },
        test("maxBy") {
          check(genCons, genFunction) { (as, f) =>
            NonEmptyList.fromCons(as).maxBy(f) <-> as.maxBy(f)
          }
        },
        test("min") {
          check(genCons) { as =>
            NonEmptyList.fromCons(as).min <-> as.min
          }
        },
        test("minBy") {
          check(genCons, genFunction) { (as, f) =>
            NonEmptyList.fromCons(as).minBy(f) <-> as.minBy(f)
          }
        },
        test("mkString") {
          check(genCons, genString, genString, genString) { (as, start, sep, end) =>
            NonEmptyList.fromCons(as).mkString(start, sep, end) <-> as.mkString(start, sep, end)
          }
        },
        test("product") {
          check(genCons) { as =>
            NonEmptyList.fromCons(as).product <-> as.product
          }
        },
        test("reduceLeft") {
          check(genCons, genFunction2) { (as, f) =>
            NonEmptyList.fromCons(as).reduceLeft(f) <-> as.reduceLeft(f)
          }
        },
        test("reduceRight") {
          check(genCons, genFunction2) { (as, f) =>
            NonEmptyList.fromCons(as).reduceRight(f) <-> as.reduceRight(f)
          }
        },
        test("reverse") {
          check(genCons) { as =>
            NonEmptyList.fromCons(as).reverse.toCons <-> as.reverse
          }
        },
        test("sum") {
          check(genCons) { as =>
            NonEmptyList.fromCons(as).sum <-> as.sum
          }
        },
        test("tails") {
          check(genCons) { as =>
            NonEmptyList.fromCons(as).tails.map(_.toCons).toCons <-> as.tails.toList.init
          }
        },
        test("take") {
          check(genConsWithIndex) { case (as, i) =>
            NonEmptyList.fromCons(as).take(i) <-> as.take(i)
          }
        },
        test("takeRight") {
          check(genConsWithIndex) { case (as, i) =>
            NonEmptyList.fromCons(as).takeRight(i) <-> as.takeRight(i)
          }
        },
        test("takeWhile") {
          check(genCons, genBooleanFunction) { (as, f) =>
            NonEmptyList.fromCons(as).takeWhile(f) <-> as.takeWhile(f)
          }
        },
        test("toCons") {
          check(genNonEmptyList) { as =>
            val cons         = as.toCons
            val nonEmptyList = NonEmptyList.fromCons(cons)
            assert(nonEmptyList)(equalTo(as))
          }
        },
        test("toString") {
          check(genCons) { as =>
            NonEmptyList.fromCons(as).toString <-> as.mkString("NonEmptyList(", ", ", ")")
          }
        },
        test("zip") {
          check(genCons, genCons) { (as, bs) =>
            val actual   = NonEmptyList.fromCons(as).zip(NonEmptyList.fromCons(bs)).toCons
            val expected = as.zip(bs)
            actual <-> expected
          }
        },
        test("zipWithIndex") {
          check(genCons) { as =>
            val actual   = NonEmptyList.fromCons(as).zipWithIndex.toCons
            val expected = as.zipWithIndex
            actual <-> expected
          }
        }
      ),
      suite("constructors")(
        test("fromCons") {
          check(genCons) { as =>
            val nonEmptyList = NonEmptyList.fromCons(as)
            val cons         = nonEmptyList.toCons
            assert(cons)(equalTo(as))
          }
        },
        test("fromNonEmptyChunk") {
          check(genNonEmptyChunk) { as =>
            val nonEmptyList  = NonEmptyList.fromNonEmptyChunk(as)
            val nonEmptyChunk = nonEmptyList.toNonEmptyChunk
            assert(nonEmptyChunk)(equalTo(as))
          }
        }
      )
    )
}
