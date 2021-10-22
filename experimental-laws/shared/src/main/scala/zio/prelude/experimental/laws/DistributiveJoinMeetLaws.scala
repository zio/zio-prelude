package zio.prelude.experimental.laws

import zio.prelude.experimental._
import zio.prelude.experimental.coherent._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object DistributiveJoinMeetLaws extends Lawful[DistributiveJoinMeetEqual] {

  /**
   * The join distributiveJoinMeet law states that for the join operator `vvv`, the meet operator `^^^`,
   * and for any value `a`, the following must hold:
   *
   * {{{
   * a1 vvv (a2 ^^^ a3) === (a1 vvv a2) ^^^ (a1 vvv a3)
   * }}}
   */
  lazy val joinDistributiveJoinMeetLaw: Laws[DistributiveJoinMeetEqual] =
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
  lazy val meetDistributiveJoinMeetLaw: Laws[DistributiveJoinMeetEqual] =
    new Laws.Law3[DistributiveJoinMeetEqual]("meetDistributiveJoinMeetLaw") {
      def apply[A](a1: A, a2: A, a3: A)(implicit A: DistributiveJoinMeetEqual[A]): TestResult =
        (a1 ^^^ (a2 vvv a3)) <-> ((a1 ^^^ a2) vvv (a1 ^^^ a3))
    }

  /**
   * The set of all laws that instances of `DistributiveJoinMeet` must satisfy.
   */
  lazy val laws: Laws[DistributiveJoinMeetEqual] =
    joinDistributiveJoinMeetLaw + meetDistributiveJoinMeetLaw

}
