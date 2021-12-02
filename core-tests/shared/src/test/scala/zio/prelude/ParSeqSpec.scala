package zio.prelude

import zio.prelude.ParSeq._
import zio.prelude.laws._
import zio.random._
import zio.test._
import zio.test.laws._

object ParSeqSpec extends DefaultRunnableSpec {

  val parSeq: Gen[Random with Sized, ParSeq[Unit, Int]] =
    Gens.parSeq(Gen.unit, Gen.anyInt)

  val equalparSeqs: Gen[Random with Sized, (ParSeq[Unit, Int], ParSeq[Unit, Int])] =
    (parSeq <*> parSeq <*> parSeq).flatMap { case ((a, b), c) =>
      Gen.elements(
        (a, a),
        (Then(Then(a, b), c), Then(a, Then(b, c))),
        (Then(a, Both(b, c)), Both(Then(a, b), Then(a, c))),
        (Both(Both(a, b), c), Both(a, Both(b, c))),
        (Both(Then(a, c), Then(b, c)), Then(Both(a, b), c)),
        (Both(a, b), Both(b, a)),
        (a, Then(a, Empty)),
        (a, Both(a, Empty))
      )
    }

  val hash = ParSeq.parSeqHash

  def spec: ZSpec[Environment, Failure] =
    suite("parSeqSpec")(
      suite("laws")(
        testM("covariant") {
          checkAllLaws(CovariantLaws)(GenFs.parSeq(Gen.unit), Gen.anyInt)
        },
        testM("hash") {
          checkAllLaws(HashLaws)(Gens.parSeq(Gen.unit, Gen.anyInt))
        },
        testM("identityBoth") {
          checkAllLaws(IdentityBothLaws)(GenFs.parSeq(Gen.unit), Gen.anyInt)
        },
        testM("identityFlatten") {
          checkAllLaws(IdentityFlattenLaws)(GenFs.parSeq(Gen.unit), Gen.anyInt)
        },
        testM("forEach") {
          checkAllLaws(ForEachLaws)(GenFs.parSeq(Gen.unit), Gen.anyInt)
        }
      ),
      suite("both")(
        testM("associative") {
          check(parSeq, parSeq, parSeq) { (a, b, c) =>
            assert((a && b) && c)(equalTo(a && (b && c))) &&
            assert(a && (b && c))(equalTo((a && b) && c))
          }
        },
        testM("commutative") {
          check(parSeq, parSeq) { (a, b) =>
            assert(a && b)(equalTo(b && a))
          }
        },
        testM("identity") {
          check(parSeq) { a =>
            assert(Both(a, empty))(equalTo(a)) &&
            assert(Both(empty, a))(equalTo(a))
          }
        }
      ),
      testM("hashCode") {
        check(equalparSeqs) { case (a, b) =>
          assert(a.hashCode)(equalTo(b.hashCode))
        }
      },
      suite("then")(
        testM("associative") {
          check(parSeq, parSeq, parSeq) { (a, b, c) =>
            assert((a ++ b) ++ c)(equalTo(a ++ (b ++ c))) &&
            assert(a ++ (b ++ c))(equalTo((a ++ b) ++ c))
          }
        },
        testM("commutative") {
          check(parSeq, parSeq, parSeq) { (a, b, c) =>
            assert(a ++ (b && c))(equalTo((a ++ b) && (a ++ c))) &&
            assert((a && b) ++ c)(equalTo((a ++ c) && (b ++ c)))
          }
        },
        testM("identity") {
          check(parSeq) { a =>
            assert(Then(a, empty))(equalTo(a)) &&
            assert(Then(empty, a))(equalTo(a))
          }
        }
      )
    )
}
