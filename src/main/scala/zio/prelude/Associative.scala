package zio.prelude

trait AssociativeLaws[A] extends ClosureLaws[A] {
  def combine(l: A, r: A): A

  final def associativityLaw(a1: A, a2: A, a3: A)(implicit equal: Equal[A]): Boolean =
    combine(a1, combine(a2, a3)) ===
      combine(combine(a1, a2), a3)
}
sealed trait Associative[A] extends AssociativeLaws[A]
object Associative {
  def apply[A](implicit associative: Associative[A]): Associative[A] = associative

  def apply[A](f: (A, A) => A): Associative[A] =
    new Associative[A] {
      def combine(l: A, r: A): A = f(l, r)
    }

}
