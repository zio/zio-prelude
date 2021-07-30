package zio.prelude

import zio.prelude.newtypes._
import zio.{Chunk, Has, NonEmptyChunk, Random, Ref}
import zio.test._
import zio.test.laws._

object ForEachSpec extends DefaultRunnableSpec {
  import Fixtures._

  val genBoolean: Gen[Has[Random], Boolean] =
    Gen.boolean

  val genInt: Gen[Has[Random], Int] =
    Gen.anyInt

  val genChunk: Gen[Has[Random] with Has[Sized], Chunk[Int]] =
    Gen.chunkOf(genInt)

  val genList: Gen[Has[Random] with Has[Sized], List[Int]] =
    Gen.listOf(genInt)

  val genBooleanFunction: Gen[Has[Random], Int => Boolean] =
    Gen.function(genBoolean)

  val genIntFunction: Gen[Has[Random], Int => Int] =
    Gen.function(genInt)

  val genIntFunction2: Gen[Has[Random], (Int, Int) => Int] =
    Gen.function2(genInt)

  val genIntIntFunction2: Gen[Has[Random], (Int, Int) => (Int, Int)] =
    Gen.function2(genInt <*> genInt)

  implicit val chunkOptionForEach: ForEach[ChunkOption] =
    ForEach[Chunk].compose[Option]

  def spec: ZSpec[Environment, Failure] =
    suite("ForEachSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(ForEach)(GenF.chunk, Gen.anyInt)),
        test("chunk . option")(checkAllLaws(ForEach)(chunkOptionGenF, Gen.anyInt)),
        test("either")(checkAllLaws(ForEach)(GenFs.either(Gen.anyInt), Gen.anyInt)),
        test("list")(checkAllLaws(ForEach)(GenF.list, Gen.anyInt)),
        test("map")(checkAllLaws(ForEach)(GenFs.map(Gen.anyInt), Gen.anyInt)),
        test("option")(checkAllLaws(ForEach)(GenF.option, Gen.anyInt)),
        test("vector")(checkAllLaws(ForEach)(GenF.vector, Gen.anyInt))
      ),
      suite("combinators")(
        test("contains") {
          check(genList, genInt) { (as, a) =>
            val actual   = ForEach[List].contains(as)(a)
            val expected = as.contains(a)
            assert(actual)(equalTo(expected))
          }
        },
        test("count") {
          check(genList, genBooleanFunction) { (as, f) =>
            val actual   = ForEach[List].count(as)(f)
            val expected = as.count(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("exists") {
          check(genList, genBooleanFunction) { (as, f) =>
            val actual   = ForEach[List].exists(as)(f)
            val expected = as.exists(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("find") {
          check(genList, genBooleanFunction) { (as, f) =>
            val actual   = ForEach[List].find(as)(f)
            val expected = as.find(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("foldLeft") {
          check(genList, genInt, genIntFunction2) { (as, s, f) =>
            val actual   = ForEach[List].foldLeft(as)(s)(f)
            val expected = as.foldLeft(s)(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("foldLeftM") {
          for {
            ref   <- Ref.make[Chunk[Int]](Chunk.empty)
            in     = List(1, 2, 3, 4, 5)
            s     <- in.foldLeftM(0)((s, a) => ref.modify(chunk => (s + a, chunk :+ a)))
            value <- ref.get
          } yield assert(s)(equalTo(15)) &&
            assert(value)(equalTo(Chunk(1, 2, 3, 4, 5)))
        },
        test("foldRight") {
          check(genList, genInt, genIntFunction2) { (as, s, f) =>
            val actual   = ForEach[List].foldRight(as)(s)(f)
            val expected = as.foldRight(s)(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("foldRightM") {
          for {
            ref   <- Ref.make[Chunk[Int]](Chunk.empty)
            in     = List(1, 2, 3, 4, 5)
            s     <- in.foldRightM(0)((a, s) => ref.modify(chunk => (s + a, chunk :+ a)))
            value <- ref.get
          } yield assert(s)(equalTo(15)) &&
            assert(value)(equalTo(Chunk(5, 4, 3, 2, 1)))
        },
        test("forall") {
          check(genList, genBooleanFunction) { (as, f) =>
            val actual   = ForEach[List].forall(as)(f)
            val expected = as.forall(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("groupByNonEmpty") {
          check(genList, genIntFunction) { (as, f) =>
            val actual   = ForEach[List].groupByNonEmpty(as)(f)
            val expected = as
              .groupBy(f)
              .toList
              .map { case (k, v) => (k, NonEmptyChunk.fromIterable(v.head, v.tail)) }
              .toMap // .toList .toMap because Scala 2.13 collections
            assert(actual)(equalTo(expected))
          }
        },
        test("groupByNonEmptyM") {
          check(genList, genIntFunction) { (as, f) =>
            // Dotty can't infer Function1Covariant: 'Required: zio.prelude.Covariant[[R] =>> Int => R]'
            val actual   = ForEach[List].groupByNonEmptyM(as)(f.map(Option(_))(Invariant.Function1Covariant))
            val expected = Option(
              as.groupBy(f)
                .toList
                .map { case (k, v) => (k, NonEmptyChunk.fromIterable(v.head, v.tail)) }
                .toMap // .toList .toMap because Scala 2.13 collections
            )
            assert(actual)(equalTo(expected))
          }
        },
        test("isEmpty") {
          check(genList) { (as) =>
            val actual   = ForEach[List].isEmpty(as)
            val expected = as.isEmpty
            assert(actual)(equalTo(expected))
          }
        },
        test("map") {
          check(genList, genIntFunction) { (as, f) =>
            val actual   = ForEach[List].map(f)(as)
            val expected = as.map(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("mapAccum") {
          check(genChunk, genInt, genIntIntFunction2) { (as, s, f) =>
            val actual   = ForEach[Chunk].mapAccum(as)(s)(f)
            val expected = as.mapAccum(s)(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("maxOption") {
          check(genList) { (as) =>
            val actual   = ForEach[List].maxOption(as)
            val expected = as.maxOption
            assert(actual)(equalTo(expected))
          }
        },
        test("maxByOption") {
          check(genList, genIntFunction) { (as, f) =>
            val actual   = ForEach[List].maxByOption(as)(f)
            val expected = as.maxByOption(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("minOption") {
          check(genList) { (as) =>
            val actual   = ForEach[List].minOption(as)
            val expected = as.minOption
            assert(actual)(equalTo(expected))
          }
        },
        test("minByOption") {
          check(genList, genIntFunction) { (as, f) =>
            val actual   = ForEach[List].minByOption(as)(f)
            val expected = as.minByOption(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("nonEmpty") {
          check(genList) { (as) =>
            val actual   = ForEach[List].nonEmpty(as)
            val expected = as.nonEmpty
            assert(actual)(equalTo(expected))
          }
        },
        test("product") {
          check(genList) { (as) =>
            val actual   = ForEach[List].product(as)
            val expected = as.product
            assert(actual)(equalTo(expected))
          }
        },
        test("reduceOption") {
          check(genList, genIntFunction2) { (as, f) =>
            val actual   = ForEach[List].reduceOption(as)(f)
            val expected = as.reduceOption(f)
            assert(actual)(equalTo(expected))
          }
        },
        test("reverse") {
          check(genList) { (as) =>
            val actual   = ForEach[List].reverse(as)
            val expected = as.reverse
            assert(actual)(equalTo(expected))
          }
        },
        test("size") {
          check(genList) { (as) =>
            val actual   = ForEach[List].size(as)
            val expected = as.size
            assert(actual)(equalTo(expected))
          }
        },
        test("sum") {
          check(genList) { (as) =>
            val actual   = ForEach[List].sum(as)
            val expected = as.sum
            assert(actual)(equalTo(expected))
          }
        },
        test("zipWithIndex") {
          check(genList) { (as) =>
            val actual   = ForEach[List].zipWithIndex(as)
            val expected = as.zipWithIndex
            assert(actual)(equalTo(expected))
          }
        }
      ),
      test("zipWithIndex is stacks safe") {
        val as       = (1 to 100000).toList
        val expected = as.zipWithIndex
        val actual   = Invariant.ListForEach.zipWithIndex(as)
        assert(actual)(equalTo(expected))
      },
      test("ForEach can be derived from Iterable") {
        check(genList, genInt, genIntFunction2) { (as, s, f) =>
          val actual   = ForEach[Seq].foldLeft(as)(s)(f)
          val expected = as.foldLeft(s)(f)
          assert(actual)(equalTo(expected))
        }
      },
      test("foldMap is stacks safe") {
        val as       = (1 to 100000).toList
        val expected = as.sum
        val actual   = as.foldMap(a => Sum(a))
        assert(actual)(equalTo(expected))
      },
      suite("foldMapM")(
        test("is stack safe") {
          def passThrough(i: Int): Either[Int, Sum[Int]] = Right(Sum(i))

          val as       = (1 to 100000).toList
          val expected = Right(Sum(as.sum))
          val actual   = as.foldMapM(passThrough)
          assert(actual)(equalTo(expected))
        },
        test("yields the right result") {
          def errorOnOddNumber(i: Int): Either[Int, Sum[Int]] =
            if (i % 2 == 0) Right(Sum(i))
            else Left(i)

          val mixedValues   = List(1, 2, 3, 4)
          val expectedMixed = Left(1)
          val actualMixed   = mixedValues.foldMapM(errorOnOddNumber)

          val evenValues   = mixedValues.filter(_ % 2 == 0)
          val expectedEven = Right(Sum(6))
          val actualEven   = evenValues.foldMapM(errorOnOddNumber)

          assert(actualMixed)(equalTo(expectedMixed)) &&
          assert(actualEven)(equalTo(expectedEven))
        },
        test("shortcircuits sideffects according to effect") {
          var calledOn: List[Int] = Nil

          def errorOnOddNumber(i: Int): Either[Int, Sum[Int]] = {
            calledOn = i :: calledOn

            if (i % 2 == 0) Right(Sum(i))
            else Left(i)
          }

          val values   = List(2, 3, 4)
          val expected = Left(3)
          val actual   = values.foldMapM(errorOnOddNumber)

          assert(actual)(equalTo(expected)) &&
          assert(calledOn)(equalTo(List(3, 2)))
        }
      )
    )
}
