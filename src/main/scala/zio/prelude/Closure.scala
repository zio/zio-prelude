package zio.prelude

package zio.prelude

sealed trait Closure[A] {
  def combine(l: A, r: A): A

  final def closureLaw(l: A, r: A): Boolean =
    try {
      combine(l, r)

      true
    } catch { case _: Throwable => false }
}
object Closure {
  def apply[A](implicit closure: Closure[A]): Closure[A] = closure

  def apply[A](f: (A, A) => A): Closure[A] =
    new Closure[A] {
      def combine(l: A, r: A): A = f(l, r)
    }

  implicit def AssociativeDerivesClosure[A](implicit associative: Associative[A]): Closure[A] =
    Closure(associative.combine(_, _))

  implicit def CommutativeDerivesClosure[A](implicit associative: Associative[A]): Closure[A] =
    Closure(associative.combine(_, _))
}
