package zio.prelude

import zio.prelude.laws._
import zio.stream.ZStream
import zio.test._
import zio.test.laws._

object EquivalenceSpec extends DefaultRunnableSpec {

  val genAny: Gen[Any, Any] =
    Gen.unit

  val genNothing: Gen[Any, Nothing] =
    Gen(ZStream.empty)

  def spec: ZSpec[Environment, Failure] =
    suite("EquivalenceSpec")(
      suite("laws")(
        test("either") {
          implicit val equivalence = Equivalence.either[Int, Int, Int]
          val left                 = Gen.either(Gen.anyInt, Gen.either(Gen.anyInt, Gen.anyInt))
          val right                = Gen.either(Gen.either(Gen.anyInt, Gen.anyInt), Gen.anyInt)
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("eitherFlip") {
          implicit val equivalence = Equivalence.eitherFlip[Int, Int]
          val left                 = Gen.either(Gen.anyInt, Gen.anyInt)
          val right                = Gen.either(Gen.anyInt, Gen.anyInt)
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("eitherNothing") {
          implicit val equal       = Equal.EitherEqual[Int, Nothing](Equal.IntHashOrd, Equal.NothingHashOrd)
          implicit val equivalence = Equivalence.eitherNothing[Int]
          val left                 = Gen.either(Gen.anyInt, genNothing)
          val right                = Gen.anyInt
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("identity") {
          implicit val equivalence = Equivalence.identity[Int]
          val left                 = Gen.anyInt
          val right                = Gen.anyInt
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("tuple") {
          implicit val equivalence = Equivalence.tuple[Int, Int, Int]
          val left                 = Gen.anyInt <*> (Gen.anyInt <*> Gen.anyInt)
          val right                = ((Gen.anyInt <*> Gen.anyInt) <*> Gen.anyInt).map { case (a, b, c) => ((a, b), c) }
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("tupleFlip") {
          implicit val equivalence = Equivalence.tupleFlip[Int, Int]
          val left                 = Gen.anyInt <*> Gen.anyInt
          val right                = Gen.anyInt <*> Gen.anyInt
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("tupleAny") {
          implicit val equal       = Equal.AnyHashOrd
          implicit val equivalence = Equivalence.tupleAny[Int]
          val left                 = Gen.anyInt <*> genAny
          val right                = Gen.anyInt
          checkAllLaws(EquivalenceLaws)(left, right)
        }
      )
    )
}
