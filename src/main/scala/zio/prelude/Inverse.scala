package zio.prelude

import zio.prelude.coherent.EqualInverse
import zio.prelude.newtypes.{ Prod, Sum }
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

  def apply[A](implicit Inverse: Inverse[A]): Inverse[A] = Inverse

  def make[A](identity0: A, op: (A, A) => A, inv: (A) => A): Inverse[A] =
    new Inverse[A] {
      def identity: A                  = identity0
      def combine(l: => A, r: => A): A = op(l, r)
      def inverse(a: A): A             = inv(a)
    }

  implicit val ByteProdInverse: Identity[Prod[Byte]] =
    Inverse.make(Prod(1), (l: Prod[Byte], r: Prod[Byte]) => Prod((l * r).toByte), a => Prod((1 / a).toByte))

  implicit val ByteSumInverse: Identity[Sum[Byte]] =
    Inverse.make(Sum(0), (l: Sum[Byte], r: Sum[Byte]) => Sum((l + r).toByte), a => Sum((-a).toByte))

  implicit val CharProdInverse: Inverse[Prod[Char]] =
    Inverse.make(Prod('\u0001'), (l, r) => Prod((l * r).toChar), a => Prod((1 / a).toChar))

  implicit val CharSumInverse: Inverse[Sum[Char]] =
    Inverse.make(Sum('\u0000'), (l, r) => Sum((l + r).toChar), a => Sum((-a).toChar))

  implicit val DoubleProdInverse: Inverse[Prod[Double]] =
    Inverse.make(Prod(1), (l: Prod[Double], r: Prod[Double]) => Prod(l * r), a => Prod(1 / a))

  implicit val DoubleSumInverse: Inverse[Sum[Double]] =
    Inverse.make(Sum(0), (l: Sum[Double], r: Sum[Double]) => Sum(l + r), a => Sum(-a))

  implicit val FloatProdInverse: Inverse[Prod[Float]] =
    Inverse.make(Prod(1), (l, r) => Prod(l * r), a => Prod(1 / a))

  implicit val FloatSumInverse: Inverse[Sum[Float]] =
    Inverse.make(Sum(0), (l, r) => Sum(l + r), a => Sum(-a))

  implicit val IntProdInverse: Inverse[Prod[Int]] =
    Inverse.make(Prod(1), (l, r) => Prod(l * r), a => Prod(1 / a))

  implicit val IntSumInverse: Inverse[Sum[Int]] =
    Inverse.make(Sum(0), (l, r) => Sum(l + r), a => Sum(-a))

  implicit val ShortProdInverse: Inverse[Prod[Short]] =
    Inverse.make(Prod(1), (l: Prod[Short], r: Prod[Short]) => Prod((l * r).toShort), a => Prod((1 / a).toShort))

  implicit val ShortSumInverse: Inverse[Sum[Short]] =
    Inverse.make(Sum(0), (l: Sum[Short], r: Sum[Short]) => Sum((l + r).toShort), a => Sum((-a).toShort))

}
