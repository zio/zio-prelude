package zio.prelude

import zio.prelude.coherent.ComplementEqual
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait Complement[A, +Join[x] <: Identity[x], +Meet[x] <: Identity[x]]
    extends BottomTopShape[A, Join, Meet]
    with ComplementShape[A, Join, Meet]

object Complement extends Lawful[ComplementEqual] {

  /**
   * The join complement law states that for the join operator `vvv`, the top element `1`, complement operator `!`
   * and for any value `a`, the following must hold:
   *
   * {{{
   * !a vvv a === 1
   * }}}
   */
  val joinComplementLaw: Laws[ComplementEqual] =
    new Laws.Law1[ComplementEqual]("joinComplementLaw") {
      def apply[A](a: A)(implicit A: ComplementEqual[A]): TestResult =
        (A.complement(a) vvv a) <-> A.top
    }

  /**
   * The meet complement law states that for the meet operator `^^^`, the bottom element `0`, complement operator `!`
   * and for any value `a`, the following must hold:
   *
   * {{{
   * !a ^^^ a === 0
   * }}}
   */
  val meetComplementLaw: Laws[ComplementEqual] =
    new Laws.Law1[ComplementEqual]("meetComplementLaw") {
      def apply[A](a: A)(implicit A: ComplementEqual[A]): TestResult =
        (!a ^^^ a) <-> A.bottom
    }

  /**
   * The set of all laws that instances of `Complement` must satisfy.
   */
  val laws: Laws[ComplementEqual] =
    joinComplementLaw + meetComplementLaw

  /**
   * Summons an implicit `Complement[A]`.
   */
  def apply[A, Join[x] <: Identity[x], Meet[x] <: Identity[x]](implicit
    complement: Complement[A, Join, Meet]
  ): Complement[A, Join, Meet] =
    complement
}
