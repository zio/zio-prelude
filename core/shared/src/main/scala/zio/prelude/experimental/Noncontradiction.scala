package zio.prelude
package experimental

import zio.prelude.experimental.coherent.NoncontradictionEqual
import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}

trait Noncontradiction[A] extends BottomShape[A] with ComplementShape[A]

object Noncontradiction extends Lawful[NoncontradictionEqual] {

  type Aux[A, +join[x] <: Identity[x], +meet[x] <: Associative[x]] = Noncontradiction[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

  /**
   * The non-contradiction law states that for the meet operator `^^^`, the bottom element `0`, complement operator `!`
   * and for any value `a`, the following must hold:
   *
   * {{{
   * !a ^^^ a === 0
   * }}}
   */
  lazy val noncontradictionLaw: Laws[NoncontradictionEqual] =
    new Laws.Law1[NoncontradictionEqual]("noncontradictionLaw") {
      def apply[A](a: A)(implicit A: NoncontradictionEqual[A]): TestResult =
        (!a ^^^ a) <-> A.bottom
    }

  /**
   * The set of all laws that instances of `Noncontradiction` must satisfy.
   */
  lazy val laws: Laws[NoncontradictionEqual] =
    noncontradictionLaw

  /**
   * Summons an implicit `Noncontradiction[A]`.
   */
  def apply[A, Join[x] <: Identity[x], Meet[x] <: Associative[x]](implicit
    noncontradiction: Noncontradiction.Aux[A, Join, Meet]
  ): Noncontradiction.Aux[A, Join, Meet] =
    noncontradiction
}
