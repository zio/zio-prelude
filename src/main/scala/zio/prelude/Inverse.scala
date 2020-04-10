package zio.prelude

import zio.prelude.coherent.EqualInverse
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait Inverse[A] extends Identity[A] {

  /**
   * Returns a right inverse for the given `A` value, such that when
   * the value is combined with the inverse (on the right hand side),
   * the identity element is returned.
   */
  def inverse(a: A): A
}

object Inverse extends Lawful[EqualInverse] {
  final val rightInverseLaw = new Laws.Law1[EqualInverse]("rightInverseLaw") {
    def apply[A](a: A)(implicit I: EqualInverse[A]): TestResult =
      I.combine(a, I.inverse(a)) <-> I.identity
  }

  def laws = rightInverseLaw + Identity.laws
}
