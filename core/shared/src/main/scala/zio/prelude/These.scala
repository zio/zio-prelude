package zio.prelude

import zio.prelude.These._

sealed trait These[+A, +B] { self =>
  def combine[A1 >: A, B1 >: B](that: These[A1, B1])(implicit A1: Associative[A1], B1: Associative[B1]): These[A1, B1] =
    (self, that) match {
      case (Left(l1), Left(l2))         => Left(A1.combine(l1, l2))
      case (Left(l), Right(r))          => Both(l, r)
      case (Left(l1), Both(l2, r))      => Both(A1.combine(l1, l2), r)
      case (Right(r), Left(l))          => Both(l, r)
      case (Right(r1), Right(r2))       => Right(B1.combine(r1, r2))
      case (Right(r1), Both(l, r2))     => Both(l, B1.combine(r1, r2))
      case (Both(l1, r), Left(l2))      => Both(A1.combine(l1, l2), r)
      case (Both(l, r1), Right(r2))     => Both(l, B1.combine(r1, r2))
      case (Both(l1, r1), Both(l2, r2)) => Both(A1.combine(l1, l2), B1.combine(r1, r2))
    }
}

object These {
  final case class Left[+A](value: A)              extends These[A, Nothing]
  final case class Right[+B](value: B)             extends These[Nothing, B]
  final case class Both[+A, +B](left: A, right: B) extends These[A, B]

  /** The `Associative` instance for `These`. */
  implicit def TheseAssociative[A: Associative, B: Associative]: Associative[These[A, B]] =
    new Associative[These[A, B]] {
      override def combine(l: => These[A, B], r: => These[A, B]): These[A, B] = l.combine(r)
    }

  /**
   * The `Bicovariant` instance for `These`.
   */
  implicit val TheseBicovariant: Bicovariant[These] = new Bicovariant[These] {
    override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): These[A, B] => These[AA, BB] = {
      case Left(value)       => Left(f(value))
      case Right(value)      => Right(g(value))
      case Both(left, right) => Both(f(left), g(right))
    }
  }

  /** The `Commutative` instance for `These`. */
  implicit def TheseCommutative[A: Commutative, B: Commutative]: Commutative[These[A, B]] =
    new Commutative[These[A, B]] {
      override def combine(l: => These[A, B], r: => These[A, B]): These[A, B] = l.combine(r)
    }

  /** The `Equal` instance for `These`. */
  implicit def TheseEqual[A: Equal, B: Equal]: Equal[These[A, B]] =
    (l: These[A, B], r: These[A, B]) =>
      (l, r) match {
        case (Left(l1), Left(l2))         => l1 === l2
        case (Right(r1), Right(r2))       => r1 === r2
        case (Both(l1, r1), Both(l2, r2)) => l1 === l2 && r1 === r2
        case _                            => false
      }

  /** The `Hash` instance for `These`. */
  implicit def TheseHash[A: Hash, B: Hash]: Hash[These[A, B]] =
    new Hash[These[A, B]] {
      override protected def checkEqual(l: These[A, B], r: These[A, B]): Boolean =
        TheseEqual(Hash[A], Hash[B]).equal(l, r)

      override def hash(a: These[A, B]): Int = a match {
        case Left(left)        => Left(left.hash).hashCode()
        case Right(right)      => Right(right.hash).hashCode()
        case Both(left, right) => Both(left.hash, right.hash).hashCode()
      }
    }

  /** The `Idempotent` instance for `These`. */
  implicit def TheseIdempotent[A: Idempotent, B: Idempotent]: Idempotent[These[A, B]] =
    new Idempotent[These[A, B]] {
      override def combine(l: => These[A, B], r: => These[A, B]): These[A, B] = l.combine(r)
    }

}
