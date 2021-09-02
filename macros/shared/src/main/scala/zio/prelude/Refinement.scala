package zio.prelude

import scala.language.implicitConversions

sealed trait Refinement[-A] { self =>
  import Refinement._

  def &&[A1 <: A](that: Refinement[A1]): Refinement[A1] = And(self, that)

  def ||[A1 <: A](that: Refinement[A1]): Refinement[A1] = Or(self, that)

  def unary_! : Refinement[A] = Not(self)

  def apply(a: A): Either[RefinementError, Unit] = self.apply(a, negated = false)

  protected def apply(a: A, negated: Boolean): Either[RefinementError, Unit]
}

object Refinement {
  lazy val always: Refinement[Any] = Refinement.Always

  def equalTo[A](value: A): Refinement[A] = EqualTo(value)

  def greaterThan[A](value: A)(implicit ordering: Ordering[A]): Refinement[A] = GreaterThan(value)

  def greaterThanOrEqualTo[A](value: A)(implicit ordering: Ordering[A]): Refinement[A] = !lessThan(value)

  def lessThan[A](value: A)(implicit ordering: Ordering[A]): Refinement[A] = LessThan(value)

  def lessThanOrEqualTo[A](value: A)(implicit ordering: Ordering[A]): Refinement[A] = !greaterThan(value)

  def matches(regex: Regex): Refinement[String] = Matches(regex)

  lazy val never: Refinement[Any] = !always

  def notEqualTo[A](value: A): Refinement[A] = !equalTo(value)

  case class And[A](left: Refinement[A], right: Refinement[A]) extends Refinement[A] {
    def apply(a: A, negated: Boolean): Either[RefinementError, Unit] =
      if (!negated) {
        (left.apply(a, negated), right.apply(a, negated)) match {
          case (Right(_), Right(_)) => Right(())
          case (Left(e1), Left(e2)) => Left(e1 ++ e2)
          case (Left(e1), _)        => Left(e1)
          case (_, Left(e2))        => Left(e2)
        }
      } else (!left || !right).apply(a, negated = false)
  }

  case class Or[A](left: Refinement[A], right: Refinement[A]) extends Refinement[A] {
    def apply(a: A, negated: Boolean): Either[RefinementError, Unit] =
      if (!negated) {
        (left.apply(a, negated), right.apply(a, negated)) match {
          case (Left(e1), Left(e2)) => Left(e1 ++ e2)
          case _                    => Right(())
        }
      } else (!left && !right).apply(a, negated = false)
  }

  case class Not[A](assertion: Refinement[A]) extends Refinement[A] {
    def apply(a: A, negated: Boolean): Either[RefinementError, Unit] =
      assertion.apply(a, !negated)
  }

  case class EqualTo[A](value: A) extends Refinement[A] {
    def apply(a: A, negated: Boolean): Either[RefinementError, Unit] =
      if (!negated) {
        if (a == value) Right(())
        else Left(RefinementError.failure(s"equalTo($value)"))
      } else {
        if (a != value) Right(())
        else Left(RefinementError.failure(s"notEqualTo($value)"))
      }
  }

  case class GreaterThan[A](value: A)(implicit ordering: Ordering[A]) extends Refinement[A] {
    def apply(a: A, negated: Boolean): Either[RefinementError, Unit] =
      if (!negated) {
        if (ordering.gt(a, value)) Right(())
        else Left(RefinementError.failure(s"greaterThan($value)"))
      } else {
        if (ordering.lteq(a, value)) Right(())
        else Left(RefinementError.failure(s"lessThanOrEqualTo($value)"))
      }
  }

  case class LessThan[A](value: A)(implicit ordering: Ordering[A]) extends Refinement[A] {
    def apply(a: A, negated: Boolean): Either[RefinementError, Unit] =
      if (!negated) {
        if (ordering.lt(a, value)) Right(())
        else Left(RefinementError.failure(s"lessThan($value)"))
      } else {
        if (ordering.gteq(a, value)) Right(())
        else Left(RefinementError.failure(s"greaterThanOrEqualTo($value)"))
      }
  }

  case class Matches(regex: Regex) extends Refinement[String] {
    def apply(a: String, negated: Boolean): Either[RefinementError, Unit] = {
      val compiled = regex.compile
      if (!negated) {
        if (compiled.r.findAllMatchIn(a).nonEmpty) Right(())
        else Left(RefinementError.Failure(s"matches($compiled)"))
      } else {
        if (compiled.r.findAllMatchIn(a).nonEmpty) Left(RefinementError.Failure(s"doesNotMatch($compiled)"))
        else Right(())
      }
    }
  }

  object Always extends Refinement[Any] {
    def apply(a: Any, negated: Boolean): Either[RefinementError, Unit] =
      if (!negated) Right(()) else Left(RefinementError.failure("never"))
  }

  sealed trait Regex { self =>
    import Regex._

    def ~(that: Regex): Regex = AndThen(self, that)

    def |(that: Regex): Regex = OrElse(self, that)

    def min(n: Int): Regex =
      self match {
        case Repeat(regex, _, max) => Repeat(regex, Some(n), max)
        case regex                 => Repeat(regex, Some(n), None)
      }

    def max(n: Int): Regex =
      self match {
        case Repeat(regex, min, _) => Repeat(regex, min, Some(n))
        case regex                 => Repeat(regex, None, Some(n))
      }

    def compile: String
  }

  object Regex {
    val anyChar: Regex         = AnyChar
    val alphanumeric: Regex    = Alphanumeric(reversed = false)
    val anything: Regex        = Anything
    val end: Regex             = End
    val nonAlphanumeric: Regex = Alphanumeric(reversed = true)
    val whitespace: Regex      = Whitespace(reversed = false)
    val nonWhitespace: Regex   = Whitespace(reversed = true)
    val digit: Regex           = Digit(reversed = false)
    val nonDigit: Regex        = Digit(reversed = true)
    val start: Regex           = Start

    implicit def literal(str: String): Regex =
      str.toList.foldLeft(anything)((acc, char) => acc ~ Literal(char))

    def anyOf(first: Char, second: Char, rest: Char*): Regex =
      CharacterSet(Set(first, second) ++ rest.toSet, reversed = false)

    def notAnyOf(first: Char, second: Char, rest: Char*): Regex =
      CharacterSet(Set(first, second) ++ rest.toSet, reversed = true)

    def inRange(start: Char, end: Char): Regex = Range(start, end, reversed = false)

    def notInRange(start: Char, end: Char): Regex = Range(start, end, reversed = true)

    case object AnyChar extends Regex {
      def compile: String = "."
    }

    case object End extends Regex {
      def compile: String = "$"
    }

    case object Anything extends Regex {
      def compile: String = ""
    }

    final case class Alphanumeric(reversed: Boolean) extends Regex {
      def compile: String = if (reversed) raw"\W" else raw"\w"
    }

    final case class Whitespace(reversed: Boolean) extends Regex {
      def compile: String = if (reversed) raw"\S" else raw"\s"
    }

    final case class Digit(reversed: Boolean) extends Regex {
      def compile: String = if (reversed) raw"\D" else raw"\d"
    }

    final case class Literal(char: Char) extends Regex {
      def compile: String = s"$char"
    }

    final case class CharacterSet(set: Set[Char], reversed: Boolean) extends Regex {
      def compile: String = set.mkString(if (reversed) "[^" else "[", "", "]")
    }

    final case class Range(start: Char, end: Char, reversed: Boolean) extends Regex {
      def compile: String = s"${if (reversed) "[^" else "["}$start-$end]"
    }

    case object Start extends Regex {
      def compile: String = "^"
    }

    final case class Repeat(regex: Regex, min: Option[Int], max: Option[Int]) extends Regex {
      def compile: String =
        (regex, min, max) match {
          case (Anything, _, _)          => anything.compile
          case (_, Some(0), Some(1))     => raw"${regex.compile}?"
          case (_, Some(min), Some(max)) => raw"${regex.compile}{$min,$max}"
          case (_, Some(0), None)        => raw"${regex.compile}*"
          case (_, Some(1), None)        => raw"${regex.compile}+"
          case (_, Some(min), None)      => raw"${regex.compile}{$min,}"
          case (_, None, Some(max))      => raw"${regex.compile}{0,$max}"
          case (_, None, None)           => regex.compile
        }
    }

    final case class AndThen(first: Regex, second: Regex) extends Regex {
      def compile: String =
        (first, second) match {
          case (first, Anything)  => first.compile
          case (Anything, second) => second.compile
          case _                  => raw"${first.compile}${second.compile}"
        }
    }

    final case class OrElse(first: Regex, second: Regex) extends Regex {
      def compile: String =
        (first, second) match {
          case (first, Anything)  => first.compile
          case (Anything, second) => second.compile
          case _                  => raw"(${first.compile}|${second.compile})"
        }
    }
  }
}
