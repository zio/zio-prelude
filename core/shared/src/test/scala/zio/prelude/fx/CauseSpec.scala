package zio.prelude.fx

import zio.prelude._
import zio.prelude.fx.Cause._
import zio.random._
import zio.test._

object CauseSpec extends DefaultRunnableSpec {

  val causes: Gen[Random with Sized, Cause[Int]] =
    Gens.causes(Gen.anyInt)

  val equalCauses: Gen[Random with Sized, (Cause[Int], Cause[Int])] =
    (causes <*> causes <*> causes).flatMap { case ((a, b), c) =>
      Gen.elements(
        (a, a),
        (Then(Then(a, b), c), Then(a, Then(b, c))),
        (Then(a, Both(b, c)), Both(Then(a, b), Then(a, c))),
        (Both(Both(a, b), c), Both(a, Both(b, c))),
        (Both(Then(a, c), Then(b, c)), Then(Both(a, b), c)),
        (Both(a, b), Both(b, a))
      )
    }

  val hash = Cause.CauseHash

  def spec: ZSpec[Environment, Failure] =
    suite("CauseSpec")(
      testM("both is associative") {
        check(causes, causes, causes) { (a, b, c) =>
          assert((a && b) && c)(equalTo(a && (b && c))) &&
          assert(a && (b && c))(equalTo((a && b) && c))
        }
      },
      testM("both is commutative") {
        check(causes, causes) { (a, b) =>
          assert(a && b)(equalTo(b && a))
        }
      },
      testM("equals is consistent with hash code") {
        check(equalCauses) { case (a, b) =>
          assert(a.hashCode)(equalTo(b.hashCode))
        }
      },
      testM("then is associative") {
        check(causes, causes, causes) { (a, b, c) =>
          assert((a ++ b) ++ c)(equalTo(a ++ (b ++ c))) &&
          assert(a ++ (b ++ c))(equalTo((a ++ b) ++ c))
        }
      },
      testM("then distributes over both") {
        check(causes, causes, causes) { (a, b, c) =>
          assert(a ++ (b && c))(equalTo((a ++ b) && (a ++ c))) &&
          assert((a && b) ++ c)(equalTo((a ++ c) && (b ++ c)))
        }
      }
    )
}
