package zio.prelude

trait OrdLaws[-A] {
  protected implicit val self: Ord[A]

  def compare(l: A, r: A): Ordering

  final def transitivityLaw1(a1: A, a2: A, a3: A): Boolean =
    (a1 < a2) && (a2 < a3) ==> (a1 < a3)

  final def transitivityLaw2(a1: A, a2: A, a3: A): Boolean =
    (a1 > a2) && (a2 > a3) ==> (a1 > a3)

  final def antisymmetryLaw1(a1: A, a2: A): Boolean =
    (a1 <= a2) && (a2 <= a1) ==> (a1 === a2)

  final def antisymmetryLaw2(a1: A, a2: A): Boolean =
    (a1 >= a2) && (a2 >= a1) ==> (a1 === a2)

  final def connexityLaw1(a1: A, a2: A): Boolean =
    (a1 <= a2) || (a2 <= a1)

  final def connexityLaw2(a1: A, a2: A): Boolean =
    (a1 >= a2) || (a2 >= a1)
}
sealed trait Ord[-A] extends OrdLaws[A] {
  protected implicit val self: Ord[A] = this
}
object Ord {
  def apply[A](implicit ord: Ord[A]): Ord[A] = ord

  def apply[A](f: (A, A) => Ordering): Ord[A] =
    new Ord[A] {
      def compare(l: A, r: A): Ordering = f(l, r)
    }

  implicit val ordString: Ord[String] = Ord[String]((l: String, r: String) =>
    if (l < r) Ordering.LessThan
    else if (l > r) Ordering.GreaterThan
    else Ordering.Equals
  )
}
trait OrdSyntax {
  implicit class OrdSyntax[A](val l: A) {
    def >(r: A)(implicit ord: Ord[A]): Boolean =
      ord.compare(l, r) === Ordering.GreaterThan

    def >=(r: A)(implicit ord: Ord[A]): Boolean =
      (l > r) || (l === r)

    def <(r: A)(implicit ord: Ord[A]): Boolean =
      ord.compare(l, r) === Ordering.LessThan

    def <=(r: A)(implicit ord: Ord[A]): Boolean =
      (l < r) || (l === r)
  }
}

sealed trait Ordering
object Ordering {
  case object LessThan    extends Ordering
  case object Equals      extends Ordering
  case object GreaterThan extends Ordering

  implicit val orderingOrdering: Ord[Ordering] =
    new Ord[Ordering] {
      def compare(l: Ordering, r: Ordering): Ordering =
        (l, r) match {
          case (LessThan, LessThan)       => Equals
          case (LessThan, _)              => LessThan
          case (Equals, Equals)           => Equals
          case (Equals, _)                => LessThan
          case (GreaterThan, GreaterThan) => Equals
          case _                          => GreaterThan
        }
    }
}
