package zio.prelude

import zio.prelude.coherent.DistributiveJoinMeetEqual
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait DistributiveJoinMeet[A, +Join[x] <: Associative[x], +Meet[x] <: Associative[x]] extends JoinMeet[A, Join, Meet]

object DistributiveJoinMeet extends Lawful[DistributiveJoinMeetEqual] {

  /**
   * The join distributiveJoinMeet law states that for the join operator `vvv`, the meet operator `^^^`,
   * and for any value `a`, the following must hold:
   *
   * {{{
   * a1 vvv (a2 ^^^ a3) === (a1 vvv a2) ^^^ (a1 vvv a3)
   * }}}
   */
  val joinDistributiveJoinMeetLaw: Laws[DistributiveJoinMeetEqual] =
    new Laws.Law3[DistributiveJoinMeetEqual]("joinDistributiveJoinMeetLaw") {
      def apply[A](a1: A, a2: A, a3: A)(implicit A: DistributiveJoinMeetEqual[A]): TestResult =
        (a1 vvv (a2 ^^^ a3)) <-> ((a1 vvv a2) ^^^ (a1 vvv a3))
    }

  /**
   * The meet distributiveJoinMeet law states that for the join operator `vvv`, the meet operator `^^^`,
   * and for any value `a`, the following must hold:
   *
   * {{{
   * a1 ^^^ (a2 vvv a3) === (a1 ^^^ a2) vvv (a1 ^^^ a3)
   * }}}
   */
  val meetDistributiveJoinMeetLaw: Laws[DistributiveJoinMeetEqual] =
    new Laws.Law3[DistributiveJoinMeetEqual]("meetDistributiveJoinMeetLaw") {
      def apply[A](a1: A, a2: A, a3: A)(implicit A: DistributiveJoinMeetEqual[A]): TestResult =
        (a1 ^^^ (a2 vvv a3)) <-> ((a1 ^^^ a2) vvv (a1 ^^^ a3))
    }

  /**
   * The set of all laws that instances of `DistributiveJoinMeet` must satisfy.
   */
  val laws: Laws[DistributiveJoinMeetEqual] =
    joinDistributiveJoinMeetLaw + meetDistributiveJoinMeetLaw

  /**
   * Summons an implicit `DistributiveJoinMeet[A]`.
   */
  def apply[A, Join[x] <: Identity[x], Meet[x] <: Associative[x]](implicit
    distributiveJoinMeet: DistributiveJoinMeet[A, Join, Meet]
  ): DistributiveJoinMeet[A, Join, Meet] =
    distributiveJoinMeet
}
