package zio.prelude

import zio.prelude.Semiring._
import zio.random._
import zio.test._

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
      testM("both is associative") {
        check(semiring, semiring, semiring) { (a, b, c) =>
          assert((a && b) && c)(equalTo(a && (b && c))) &&
          assert(a && (b && c))(equalTo((a && b) && c))
        }
      },
      testM("both is commutative") {
        check(semiring, semiring) { (a, b) =>
          assert(a && b)(equalTo(b && a))
        }
      },
      testM("equals is consistent with hash code") {
        check(equalSemirings) { case (a, b) =>
          assert(a.hashCode)(equalTo(b.hashCode))
        }
      },
      testM("then is associative") {
        check(semiring, semiring, semiring) { (a, b, c) =>
          assert((a ++ b) ++ c)(equalTo(a ++ (b ++ c))) &&
          assert(a ++ (b ++ c))(equalTo((a ++ b) ++ c))
        }
      },
      testM("then distributes over both") {
        check(semiring, semiring, semiring) { (a, b, c) =>
          assert(a ++ (b && c))(equalTo((a ++ b) && (a ++ c))) &&
          assert((a && b) ++ c)(equalTo((a ++ c) && (b ++ c)))
        }
      }
    )
}
