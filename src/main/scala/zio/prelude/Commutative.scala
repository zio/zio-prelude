package zio.prelude

trait Commutative[A] {
  def combine(l: A, r: A): A

  final def commutativeLaw(a1: A, a2: A)(implicit equal: Equal[A]): Boolean =
    combine(a1, a2) === combine(a2, a1)
}
object Commutative {
  def apply[A](implicit commutative: Commutative[A]): Commutative[A] = commutative

  def apply[A](f: (A, A) => A): Commutative[A] =
    new Commutative[A] {
      def combine(l: A, r: A): A = f(l, r)
    }
}
