package zio.prelude.refined

import scala.language.implicitConversions

sealed trait Assertion[-A] { self =>
  import Assertion._

  def &&[A1 <: A](that: Assertion[A1]): Assertion[A1] = And(self, that)

  def ||[A1 <: A](that: Assertion[A1]): Assertion[A1] = Or(self, that)

  def unary_! : Assertion[A] = Not(self)

  def apply(a: A): Either[AssertionError, Unit] = self.apply(a, negated = false)

  protected def apply(a: A, negated: Boolean): Either[AssertionError, Unit]
}

object Assertion {
  lazy val always: Assertion[Any] = Assertion.Always

  def equalTo[A](value: A): Assertion[A] = EqualTo(value)

  def greaterThan[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = GreaterThan(value)

  def greaterThanOrEqualTo[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = !lessThan(value)

  def lessThan[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = LessThan(value)

  def lessThanOrEqualTo[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = !greaterThan(value)

  def matches(regex: Regex): Assertion[String] = Matches(regex)

  lazy val never: Assertion[Any] = !always

  def notEqualTo[A](value: A): Assertion[A] = !equalTo(value)

  final case class And[A](left: Assertion[A], right: Assertion[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        (left.apply(a, negated), right.apply(a, negated)) match {
          case (Right(_), Right(_)) => Right(())
          case (Left(e1), Left(e2)) => Left(e1 ++ e2)
          case (Left(e1), _)        => Left(e1)
          case (_, Left(e2))        => Left(e2)
        }
      } else (!left || !right).apply(a, negated = false)
  }

  final case class Or[A](left: Assertion[A], right: Assertion[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        (left.apply(a, negated), right.apply(a, negated)) match {
          case (Left(e1), Left(e2)) => Left(e1 ++ e2)
          case _                    => Right(())
        }
      } else (!left && !right).apply(a, negated = false)
  }

  final case class Not[A](assertion: Assertion[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      assertion.apply(a, !negated)
  }

  final case class EqualTo[A](value: A) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        if (a == value) Right(())
        else Left(AssertionError.failure(s"equalTo($value)"))
      } else {
        if (a != value) Right(())
        else Left(AssertionError.failure(s"notEqualTo($value)"))
      }
  }

  final case class GreaterThan[A](value: A)(implicit ordering: Ordering[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        if (ordering.gt(a, value)) Right(())
        else Left(AssertionError.failure(s"greaterThan($value)"))
      } else {
        if (ordering.lteq(a, value)) Right(())
        else Left(AssertionError.failure(s"lessThanOrEqualTo($value)"))
      }
  }

  final case class LessThan[A](value: A)(implicit ordering: Ordering[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        if (ordering.lt(a, value)) Right(())
        else Left(AssertionError.failure(s"lessThan($value)"))
      } else {
        if (ordering.gteq(a, value)) Right(())
        else Left(AssertionError.failure(s"greaterThanOrEqualTo($value)"))
      }
  }

  final case class Matches(regex: Regex) extends Assertion[String] {
    def apply(a: String, negated: Boolean): Either[AssertionError, Unit] = {
      val compiled = regex.compile
      if (!negated) {
        if (compiled.r.findAllMatchIn(a).nonEmpty) Right(())
        else Left(AssertionError.Failure(s"matches($compiled)"))
      } else {
        if (compiled.r.findAllMatchIn(a).nonEmpty) Left(AssertionError.Failure(s"doesNotMatch($compiled)"))
        else Right(())
      }
    }
  }

  case object Always extends Assertion[Any] {
    def apply(a: Any, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) Right(()) else Left(AssertionError.failure("never"))
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
          case _                  => raw"${first.compile}|${second.compile}"
        }
    }
  }
}
