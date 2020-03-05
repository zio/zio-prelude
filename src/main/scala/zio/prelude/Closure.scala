package zio.prelude

sealed trait Closure[A] {
  def combine(l: A, r: A): A

  final def closureLaw(l: A, r: A): Boolean =
    try {
      combine(l, r)

      true
    } catch { case _: Throwable => false }
}
object Closure extends ClosureImplicits0 {
  def apply[A](implicit closure: Closure[A]): Closure[A] = closure

  def apply[A](f: (A, A) => A): Closure[A] =
    new Closure[A] {
      def combine(l: A, r: A): A = f(l, r)
    }
}
private[prelude] trait ClosureImplicits1 {
  implicit def CommutativeDerivesClosure[A](implicit commutative: Commutative[A]): Closure[A] =
    Closure(commutative.combine(_, _))
}
private[prelude] trait ClosureImplicits0 {
  implicit def AssociativeDerivesClosure[A](implicit associative: Associative[A]): Closure[A] =
    Closure(associative.combine(_, _))
}
trait ClosureSyntax {
  implicit class ClosureSyntax[A](l: A) {
    def combine(r: A)(implicit closure: Closure[A]): A = closure.combine(l, r)

    def <>(r: A)(implicit closure: Closure[A]): A = closure.combine(l, r)
  }
}
