package zio.prelude

import zio.prelude.Semiring._
import zio.random._
import zio.test._
import zio.test.laws._

object SemiringSpec extends DefaultRunnableSpec {

  val semiring: Gen[Random with Sized, Semiring[Int]] =
    Gens.semiring(Gen.anyInt)

  val equalSemirings: Gen[Random with Sized, (Semiring[Int], Semiring[Int])] =
    (semiring <*> semiring <*> semiring).flatMap { case ((a, b), c) =>
      Gen.elements(
        (a, a),
        (Then(Then(a, b), c), Then(a, Then(b, c))),
        (Then(a, Both(b, c)), Both(Then(a, b), Then(a, c))),
        (Both(Both(a, b), c), Both(a, Both(b, c))),
        (Both(Then(a, c), Then(b, c)), Then(Both(a, b), c)),
        (Both(a, b), Both(b, a))
      )
    }

  val hash = Semiring.SemiringHash

  def spec: ZSpec[Environment, Failure] =
    suite("SemiringSpec")(
      suite("laws")(
        testM("covariant") {
          checkAllLaws(Covariant)(GenFs.semiring, Gen.anyInt)
        },
        testM("hash") {
          checkAllLaws(Hash)(Gens.semiring(Gen.anyInt))
        },
        testM("identityBoth") {
          checkAllLaws(IdentityBoth)(GenFs.semiring, Gen.anyInt)
        },
        testM("identityFlatten") {
          checkAllLaws(IdentityFlatten)(GenFs.semiring, Gen.anyInt)
        },
        testM("nonEmptyTraversable") {
          checkAllLaws(NonEmptyTraversable)(GenFs.semiring, Gen.anyInt)
        }
      ),
      suite("both")(
        testM("associative") {
          check(semiring, semiring, semiring) { (a, b, c) =>
            assert((a && b) && c)(equalTo(a && (b && c))) &&
            assert(a && (b && c))(equalTo((a && b) && c))
          }
        },
        testM("commutative") {
          check(semiring, semiring) { (a, b) =>
            assert(a && b)(equalTo(b && a))
          }
        }
      ),
      testM("hashCode") {
        check(equalSemirings) { case (a, b) =>
          assert(a.hashCode)(equalTo(b.hashCode))
        }
      },
      suite("then")(
        testM("associative") {
          check(semiring, semiring, semiring) { (a, b, c) =>
            assert((a ++ b) ++ c)(equalTo(a ++ (b ++ c))) &&
            assert(a ++ (b ++ c))(equalTo((a ++ b) ++ c))
          }
        },
        testM("commutative") {
          check(semiring, semiring, semiring) { (a, b, c) =>
            assert(a ++ (b && c))(equalTo((a ++ b) && (a ++ c))) &&
            assert((a && b) ++ c)(equalTo((a ++ c) && (b ++ c)))
          }
        }
      )
    )
}
