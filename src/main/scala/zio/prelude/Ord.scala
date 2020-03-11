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

  final def complementLaw(a1: A, a2: A): Boolean =
    (a1 <= a2) === (a2 >= a1)
}
sealed trait Ord[-A] extends OrdLaws[A] {
  protected implicit val self: Ord[A] = this

  def contramap[A1](f: A1 => A): Ord[A1] = Ord((l: A1, r: A1) => self.compare(f(l), f(r)))

  def mapOrdering(f: Ordering => Ordering): Ord[A] = Ord((l: A, r: A) => f(self.compare(l, r)))

  def reverse: Ord[A] = mapOrdering(_.opposite)
}
object Ord {
  def apply[A](implicit ord: Ord[A]): Ord[A] = ord

  def apply[A](f: (A, A) => Ordering): Ord[A] =
    new Ord[A] {
      def compare(l: A, r: A): Ordering = f(l, r)
    }

  implicit val ordInt: Ord[Int] =
    Ord[Int]((l: Int, r: Int) => if (l < r) Ordering.LessThan else if (l > r) Ordering.GreaterThan else Ordering.Equals)

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

    def =?=(r: A)(implicit ord: Ord[A]): Ordering = ord.compare(l, r)
  }
}

sealed trait Ordering { self =>
  def ordinal: Int = self match {
    case Ordering.LessThan    => 0
    case Ordering.Equals      => 1
    case Ordering.GreaterThan => 2
  }

  def opposite: Ordering = self match {
    case Ordering.LessThan    => Ordering.GreaterThan
    case Ordering.Equals      => Ordering.Equals
    case Ordering.GreaterThan => Ordering.LessThan
  }
}
object Ordering {
  case object LessThan    extends Ordering
  case object Equals      extends Ordering
  case object GreaterThan extends Ordering

  implicit val orderingOrdering: Ord[Ordering] =
    Ord((l: Ordering, r: Ordering) => Ord[Int].compare(l.ordinal, r.ordinal))
}
