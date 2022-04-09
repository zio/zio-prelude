package zio.prelude

import zio.prelude.laws._
import zio.stream.ZStream
import zio.test._
import zio.test.laws._

object EquivalenceSpec extends ZIOSpecDefault {

  val genAny: Gen[Any, Any] =
    Gen.unit

  val genNothing: Gen[Any, Nothing] =
    Gen(ZStream.empty)

  def spec: ZSpec[Environment, Any] =
    suite("EquivalenceSpec")(
      suite("laws")(
        test("either") {
          implicit val equivalence = Equivalence.either[Int, Int, Int]
          val left                 = Gen.either(Gen.int, Gen.either(Gen.int, Gen.int))
          val right                = Gen.either(Gen.either(Gen.int, Gen.int), Gen.int)
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("eitherFlip") {
          implicit val equivalence = Equivalence.eitherFlip[Int, Int]
          val left                 = Gen.either(Gen.int, Gen.int)
          val right                = Gen.either(Gen.int, Gen.int)
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("eitherNothing") {
          implicit val equal       = Equal.EitherEqual[Int, Nothing](Equal.IntHashOrd, Equal.NothingHashOrd)
          implicit val equivalence = Equivalence.eitherNothing[Int]
          val left                 = Gen.either(Gen.int, genNothing)
          val right                = Gen.int
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("identity") {
          implicit val equivalence = Equivalence.identity[Int]
          val left                 = Gen.int
          val right                = Gen.int
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("tuple") {
          implicit val equivalence = Equivalence.tuple[Int, Int, Int]
          val left                 = Gen.int <*> (Gen.int <*> Gen.int)
          val right                = ((Gen.int <*> Gen.int) <*> Gen.int).map { case (a, b, c) => ((a, b), c) }
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("tupleFlip") {
          implicit val equivalence = Equivalence.tupleFlip[Int, Int]
          val left                 = Gen.int <*> Gen.int
          val right                = Gen.int <*> Gen.int
          checkAllLaws(EquivalenceLaws)(left, right)
        },
        test("tupleAny") {
          implicit val equal       = Equal.AnyHashOrd
          implicit val equivalence = Equivalence.tupleAny[Int]
          val left                 = Gen.int <*> genAny
          val right                = Gen.int
          checkAllLaws(EquivalenceLaws)(left, right)
        }
      )
    )
}
