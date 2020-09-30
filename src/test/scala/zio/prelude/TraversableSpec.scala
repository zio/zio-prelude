package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.laws._
import zio.{ Chunk, NonEmptyChunk }

object TraversableSpec extends DefaultRunnableSpec {

  val genBoolean: Gen[Random, Boolean] =
    Gen.boolean

  val genInt: Gen[Random, Int] =
    Gen.anyInt

  val genChunk: Gen[Random with Sized, Chunk[Int]] =
    Gen.chunkOf(genInt)

  val genList: Gen[Random with Sized, List[Int]] =
    Gen.listOf(genInt)

  val genBooleanFunction: Gen[Random, Int => Boolean] =
    Gen.function(genBoolean)

  val genIntFunction: Gen[Random, Int => Int] =
    Gen.function(genInt)

  val genIntFunction2: Gen[Random, (Int, Int) => Int] =
    Gen.function2(genInt)

  val genIntIntFunction2: Gen[Random, (Int, Int) => (Int, Int)] =
    Gen.function2(genInt <*> genInt)

  def spec: ZSpec[Environment, Failure] =
    suite("TraversableSpec")(
      suite("instances")(
        testM("chunk")(checkAllLaws(Traversable)(GenF.chunk, Gen.anyInt)),
        testM("either")(checkAllLaws(Traversable)(GenFs.either(Gen.anyInt), Gen.anyInt)),
        testM("list")(checkAllLaws(Traversable)(GenF.list, Gen.anyInt)),
        testM("map")(checkAllLaws(Traversable)(GenFs.map(Gen.anyInt), Gen.anyInt)),
        testM("option")(checkAllLaws(Traversable)(GenF.option, Gen.anyInt)),
        testM("vector")(checkAllLaws(Traversable)(GenF.vector, Gen.anyInt))
      ),
      suite("combinators")(
        testM("contains") {
          check(genList, genInt) { (as, a) =>
            val actual   = Traversable[List].contains(as)(a)
            val expected = as.contains(a)
            assert(actual)(equalTo(expected))
          }
        },
        testM("count") {
          check(genList, genBooleanFunction) { (as, f) =>
            val actual   = Traversable[List].count(as)(f)
            val expected = as.count(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("exists") {
          check(genList, genBooleanFunction) { (as, f) =>
            val actual   = Traversable[List].exists(as)(f)
            val expected = as.exists(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("find") {
          check(genList, genBooleanFunction) { (as, f) =>
            val actual   = Traversable[List].find(as)(f)
            val expected = as.find(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("foldLeft") {
          check(genList, genInt, genIntFunction2) { (as, s, f) =>
            val actual   = Traversable[List].foldLeft(as)(s)(f)
            val expected = as.foldLeft(s)(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("foldRight") {
          check(genList, genInt, genIntFunction2) { (as, s, f) =>
            val actual   = Traversable[List].foldRight(as)(s)(f)
            val expected = as.foldRight(s)(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("forall") {
          check(genList, genBooleanFunction) { (as, f) =>
            val actual   = Traversable[List].forall(as)(f)
            val expected = as.forall(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("groupBy") {
          check(genList, genIntFunction) { (as, f) =>
            val actual   = Traversable[List].groupBy(as)(f)
            val expected = as
              .groupBy(f)
              .toList
              .map { case (k, v) => (k, NonEmptyChunk.fromIterable(v.head, v.tail)) }
              .toMap // .toList .toMap because Scala 2.13 collections
            assert(actual)(equalTo(expected))
          }
        },
        testM("groupByM") {
          check(genList, genIntFunction) { (as, f) =>
            // Dotty can't infer Function1Covariant: 'Required: zio.prelude.Covariant[[R] =>> Int => R]'
            val actual   = Traversable[List].groupByM(as)(f.map(Option(_))(Invariant.Function1Covariant))
            val expected = Option(
              as.groupBy(f)
                .toList
                .map { case (k, v) => (k, NonEmptyChunk.fromIterable(v.head, v.tail)) }
                .toMap // .toList .toMap because Scala 2.13 collections
            )
            assert(actual)(equalTo(expected))
          }
        },
        testM("isEmpty") {
          check(genList) { (as) =>
            val actual   = Traversable[List].isEmpty(as)
            val expected = as.isEmpty
            assert(actual)(equalTo(expected))
          }
        },
        testM("map") {
          check(genList, genIntFunction) { (as, f) =>
            val actual   = Traversable[List].map(f)(as)
            val expected = as.map(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("mapAccum") {
          check(genChunk, genInt, genIntIntFunction2) { (as, s, f) =>
            val actual   = Traversable[Chunk].mapAccum(as)(s)(f)
            val expected = as.mapAccum(s)(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("maxOption") {
          check(genList) { (as) =>
            val actual   = Traversable[List].maxOption(as)
            val expected = as.maxOption
            assert(actual)(equalTo(expected))
          }
        },
        testM("maxByOption") {
          check(genList, genIntFunction) { (as, f) =>
            val actual   = Traversable[List].maxByOption(as)(f)
            val expected = as.maxByOption(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("minOption") {
          check(genList) { (as) =>
            val actual   = Traversable[List].minOption(as)
            val expected = as.minOption
            assert(actual)(equalTo(expected))
          }
        },
        testM("minByOption") {
          check(genList, genIntFunction) { (as, f) =>
            val actual   = Traversable[List].minByOption(as)(f)
            val expected = as.minByOption(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("nonEmpty") {
          check(genList) { (as) =>
            val actual   = Traversable[List].nonEmpty(as)
            val expected = as.nonEmpty
            assert(actual)(equalTo(expected))
          }
        },
        testM("product") {
          check(genList) { (as) =>
            val actual   = Traversable[List].product(as)
            val expected = as.product
            assert(actual)(equalTo(expected))
          }
        },
        testM("reduceOption") {
          check(genList, genIntFunction2) { (as, f) =>
            val actual   = Traversable[List].reduceOption(as)(f)
            val expected = as.reduceOption(f)
            assert(actual)(equalTo(expected))
          }
        },
        testM("reverse") {
          check(genList) { (as) =>
            val actual   = Traversable[List].reverse(as)
            val expected = as.reverse
            assert(actual)(equalTo(expected))
          }
        },
        testM("size") {
          check(genList) { (as) =>
            val actual   = Traversable[List].size(as)
            val expected = as.size
            assert(actual)(equalTo(expected))
          }
        },
        testM("sum") {
          check(genList) { (as) =>
            val actual   = Traversable[List].sum(as)
            val expected = as.sum
            assert(actual)(equalTo(expected))
          }
        },
        testM("zipWithIndex") {
          check(genList) { (as) =>
            val actual   = Traversable[List].zipWithIndex(as)
            val expected = as.zipWithIndex
            assert(actual)(equalTo(expected))
          }
        }
      ),
      test("zipWithIndex is stacks safe") {
        val as       = (1 to 100000).toList
        val expected = as.zipWithIndex
        val actual   = Invariant.ListTraversable.zipWithIndex(as)
        assert(actual)(equalTo(expected))
      },
      testM("Traversable can be derived from Iterable") {
        check(genList, genInt, genIntFunction2) { (as, s, f) =>
          val actual   = Traversable[Seq].foldLeft(as)(s)(f)
          val expected = as.foldLeft(s)(f)
          assert(actual)(equalTo(expected))
        }
      }
    )
}
