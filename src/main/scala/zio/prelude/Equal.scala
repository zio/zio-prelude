package zio.prelude

trait EqualLaws[-A] {
  def equal(l: A, r: A): Boolean

  final def notEqual(l: A, r: A): Boolean = !equal(l, r)

  final def reflexiveLaw(a: A): Boolean = equal(a, a)

  final def symmetryLaw(a1: A, a2: A): Boolean = equal(a2, a1) ==> equal(a1, a2)

  final def transitivityLaw(a1: A, a2: A, a3: A): Boolean =
    equal(a1, a2) && equal(a2, a3) ==> equal(a1, a3)
}
sealed trait Equal[-A] extends EqualLaws[A]
object Equal extends EqualImplicits0 {
  def apply[A](implicit equal: Equal[A]): Equal[A] = equal

  def apply[A](eq0: (A, A) => Boolean): Equal[A] =
    new Equal[A] {
      def equal(l: A, r: A): Boolean = refEq(l, r) || eq0(l, r)
    }

  def default[A]: Equal[A] = Equal((l: A, r: A) => l == r)

  implicit val UnitEqual: Equal[Unit]       = Equal((_, _) => true)
  implicit val NothingEqual: Equal[Nothing] = Equal[Nothing]((l: Nothing, _: Nothing) => l)
  implicit val IntEqual: Equal[Int]         = default[Int]
  implicit val DoubleEqual: Equal[Double]   = Equal(_ == _) // FIXME

  private def refEq[A](l: A, r: A): Boolean =
    l.asInstanceOf[AnyRef] eq r.asInstanceOf[AnyRef]
}
trait EqualImplicits0 {
  implicit def OrdDerivesEqual[A](implicit ord: Ord[A]): Equal[A] =
    Equal((l, r) => ord.compare(l, r) eq Ordering.Equals)
}
trait EqualSyntax {
  implicit class EqualSyntax[A](l: A) {
    def ===(r: A)(implicit equal: Equal[A]): Boolean = equal.equal(l, r)

    def !==(r: A)(implicit equal: Equal[A]): Boolean = equal.notEqual(l, r)
  }
}
