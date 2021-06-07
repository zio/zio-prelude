package zio.prelude.refined

import zio.prelude.refined.Assertion.{And, Not, Or}

sealed trait Assertion[-A] { self =>
  def &&[A1 <: A](that: Assertion[A1]): Assertion[A1] = And(this, that)

  def ||[A1 <: A](that: Assertion[A1]): Assertion[A1] = Or(this, that)

  def unary_! : Assertion[A] = Not(this)

  def apply(a: A): Either[AssertionError, Unit] = apply(a, negated = false)
  protected def apply(a: A, negated: Boolean): Either[AssertionError, Unit]
}

object Assertion {
  def equalTo[A](value: A): Assertion[A]               = EqualTo(value)
  def greaterThan[A: Ordering](value: A): Assertion[A] = GreaterThan(value)
  def lessThan[A: Ordering](value: A): Assertion[A]    = LessThan(value)
  def matches(regex: String): Assertion[String]        = Matches(regex)

  case class And[A](left: Assertion[A], right: Assertion[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      (left.apply(a, negated), right.apply(a, negated)) match {
        case (Right(_), Right(_)) => Right(())
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case (Left(e1), _)        => Left(e1)
        case (_, Left(e2))        => Left(e2)
      }
  }

  case class Or[A](left: Assertion[A], right: Assertion[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      (left.apply(a, negated), right.apply(a, negated)) match {
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case _                    => Right(())
      }
  }

  case class Not[A](assertion: Assertion[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      assertion.apply(a, !negated)
  }

  case class EqualTo[A](value: A) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (a == value) Right(())
      else Left(AssertionError.Failure(s"equalTo($value)", a.toString))
  }

  case class GreaterThan[A](value: A)(implicit val ordering: Ordering[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (ordering.gt(a, value)) Right(())
      else Left(AssertionError.Failure(s"greaterThan($value)", a.toString))

  }

  case class LessThan[A](value: A)(implicit ordering: Ordering[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (ordering.lt(a, value)) Right(())
      else Left(AssertionError.Failure(s"greaterThan($value)", a.toString))
  }

//  case class LessThanEqual[A](value: A)    extends Assertion[A]
//  case class GreaterThanEqual[A](value: A) extends Assertion[A]
//  case object True extends Assertion[Any]

  case class Matches(regex: String) extends Assertion[String] {
    override def apply(a: String, negated: Boolean): Either[AssertionError, Unit] =
      if (regex.r.matches(a)) Right(())
      else Left(AssertionError.Failure(s"""matches("$regex")""", a))
  }
}
