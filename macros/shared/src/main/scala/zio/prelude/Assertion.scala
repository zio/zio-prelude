package zio.prelude

import scala.util.matching

sealed trait Assertion[-A] { self =>
  import Assertion._

  def &&[A1 <: A](that: Assertion[A1]): Assertion[A1] = And(self, that)

  def ||[A1 <: A](that: Assertion[A1]): Assertion[A1] = Or(self, that)

  def unary_! : Assertion[A] = Not(self)

  def apply(a: A): Either[AssertionError, Unit] = self.apply(a, negated = false)

  protected def apply(a: A, negated: Boolean): Either[AssertionError, Unit]
}

/**
 * An `Assertion[A]` is essentially a composable predicate from `A => Boolean`.
 * They can be composed with standard Boolean operators of `&&`, `||` and `!`.
 * This is primarily intended to be used with `Newtype` and `Subtype`,
 * enhancing them with compile-time time validation.
 *
 * For example, if you'd like to validate that a particular Int is precisely
 * 4 digits long, you can create the following refined Newtype. (Note that the
 * syntax is slightly difference between Scala 2 and Scala 3).
 *
 * {{{
 *  type Pin = Pin.Type
 *  object Pin extends Newtype[Int] {
 *    // Scala 2 Syntax
 *    def assertion =
 *      assert(Assertion.between(1000, 9999))
 *
 *    // Scala 3 Syntax
 *    override inline def assertion =
 *      Assertion.between(1000, 9999)
 *  }
 *
 *  // PowerOfTwo(1000) compiles
 *  // PowerOfTwo(5412) compiles
 *  // PowerOfTwo(34567) fails with "34567 did not satisfy between(1000, 9999)"
 *  // PowerOfTwo(234) fails with "123 did not satisfy between(1000, 9999)"
 * }}}
 */
object Assertion {
  val anything: Assertion[Any] = Assertion.Anything

  /**
   * Ensures the value falls between a given min and max (inclusive).
   */
  def between[A](min: A, max: A)(implicit ordering: Ordering[A]): Assertion[A] = Between(min, max)

  def divisibleBy[A](n: A)(implicit numeric: Numeric[A]): Assertion[A] = DivisibleBy(n)

  def contains(string: String): Assertion[String] = Contains(string)

  def equalTo[A](value: A): Assertion[A] = EqualTo(value)

  def endsWith(suffix: String): Assertion[String] = EndsWith(suffix)

  def greaterThan[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = GreaterThan(value)

  def greaterThanOrEqualTo[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = !lessThan(value)

  def hasLength(lengthAssertion: Assertion[Int]): Assertion[String] = HasLength(lengthAssertion)

  val isEmptyString: Assertion[String] = hasLength(equalTo(0))

  def lessThan[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = LessThan(value)

  def lessThanOrEqualTo[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = !greaterThan(value)

  def matches(regex: Regex): Assertion[String] = Matches(regex.compile)

  def matches(regexString: String): Assertion[String] = Matches(regexString)

  /**
   * Matches a [[scala.util.matching.Regex]].
   *
   * In order to use this for compile-time Assertions, make sure to use the
   * string literal extension method, e.g.:
   *
   * {{{
   *   Assertion.matches("\\w+@\\d{3,5}".r)
   * }}}
   */
  def matches(regex: matching.Regex): Assertion[String] = Matches(regex.regex)

  val never: Assertion[Any] = !anything

  def notEqualTo[A](value: A): Assertion[A] = !equalTo(value)

  /**
   * Ensures that the value is a power of the given base.
   *
   * {{{
   *  type PowerOfTwo = PowerOfTwo.Type
   *  object PowerOfTwo extends Newtype[Int] {
   *    def assertion =
   *      assert(Assertion.powerOf(2))
   *  }
   *
   *  // PowerOfTwo(1024) compiles
   *  // PowerOfTwo(1025) fails
   * }}}
   */
  def powerOf[A](base: A)(implicit numeric: Numeric[A]): Assertion[A] = PowerOf(base)

  def startsWith(prefix: String): Assertion[String] = StartsWith(prefix)

  private[prelude] case class And[A](left: Assertion[A], right: Assertion[A]) extends Assertion[A] {
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

  private[prelude] case class Or[A](left: Assertion[A], right: Assertion[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        (left.apply(a, negated), right.apply(a, negated)) match {
          case (Left(e1), Left(e2)) => Left(e1 ++ e2)
          case _                    => Right(())
        }
      } else (!left && !right).apply(a, negated = false)
  }

  private[prelude] case class Not[A](assertion: Assertion[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      assertion.apply(a, !negated)
  }

  private[prelude] case class DivisibleBy[A](n: A)(implicit numeric: Numeric[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] = {
      val result = numeric.toDouble(a) % numeric.toDouble(n) == 0
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.Failure(s"divisibleBy($n)"))
      } else {
        if (!result) Left(AssertionError.Failure(s"notDivisibleBy($n)"))
        else Right(())
      }
    }
  }

  private[prelude] case class Contains(string: String) extends Assertion[String] {
    def apply(a: String, negated: Boolean): Either[AssertionError, Unit] = {
      val result = a.contains(string)
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.Failure(s"contains($string)"))
      } else {
        if (result) Left(AssertionError.Failure(s"doesNotContain($string)"))
        else Right(())
      }
    }
  }

  private[prelude] case class EndsWith(suffix: String) extends Assertion[String] {
    def apply(a: String, negated: Boolean): Either[AssertionError, Unit] = {
      val result = a.endsWith(suffix)
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.Failure(s"startsWith($suffix)"))
      } else {
        if (result) Left(AssertionError.Failure(s"doesNotStartWith($suffix)"))
        else Right(())
      }
    }
  }

  private[prelude] case class EqualTo[A](value: A) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        if (a == value) Right(())
        else Left(AssertionError.failure(s"equalTo($value)"))
      } else {
        if (a != value) Right(())
        else Left(AssertionError.failure(s"notEqualTo($value)"))
      }
  }

  private[prelude] case class Between[A](min: A, max: A)(implicit ordering: Ordering[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] = {
      val result = ordering.gteq(a, min) && ordering.lteq(a, max)
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.failure(s"between($min, $max)"))
      } else {
        if (!result) Right(())
        else Left(AssertionError.failure(s"notBetween($min, $max)"))
      }
    }
  }

  private[prelude] case class GreaterThan[A](value: A)(implicit ordering: Ordering[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        if (ordering.gt(a, value)) Right(())
        else Left(AssertionError.failure(s"greaterThan($value)"))
      } else {
        if (ordering.lteq(a, value)) Right(())
        else Left(AssertionError.failure(s"lessThanOrEqualTo($value)"))
      }
  }

  private[prelude] case class HasLength[A](lengthAssertion: Assertion[Int]) extends Assertion[String] {
    def apply(string: String, negated: Boolean): Either[AssertionError, Unit] =
      lengthAssertion(string.length, negated) match {
        case Left(AssertionError.Failure(condition)) => Left(AssertionError.failure(s"hasLength($condition)"))
        case other                                   => other
      }
  }

  private[prelude] case class LessThan[A](value: A)(implicit ordering: Ordering[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        if (ordering.lt(a, value)) Right(())
        else Left(AssertionError.failure(s"lessThan($value)"))
      } else {
        if (ordering.gteq(a, value)) Right(())
        else Left(AssertionError.failure(s"greaterThanOrEqualTo($value)"))
      }
  }

  private[prelude] case class Matches(regexString: String) extends Assertion[String] {
    def apply(a: String, negated: Boolean): Either[AssertionError, Unit] = {
      val result = a.matches(regexString)
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.Failure(s"matches(${regexString.r})"))
      } else {
        if (result) Left(AssertionError.Failure(s"doesNotMatch(${regexString.r})"))
        else Right(())
      }
    }
  }

  private[prelude] case class PowerOf[A](base: A)(implicit numeric: Numeric[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] = {
      val result = isPower(numeric.toDouble(base), numeric.toDouble(a))
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.Failure(s"powerOf($base)"))
      } else {
        if (!result) Left(AssertionError.Failure(s"notPowerOf($base)"))
        else Right(())
      }
    }

    private def isPower(base: Double, number: Double): Boolean = {
      if (base == 1) return number == 1
      var pow = 1.0
      while (pow < number) pow = pow * base
      pow == number
    }
  }

  private[prelude] case class StartsWith(prefix: String) extends Assertion[String] {
    def apply(a: String, negated: Boolean): Either[AssertionError, Unit] = {
      val result = a.startsWith(prefix)
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.Failure(s"startsWith($prefix)"))
      } else {
        if (result) Left(AssertionError.Failure(s"doesNotStartWith($prefix)"))
        else Right(())
      }
    }
  }

  private[prelude] object Anything extends Assertion[Any] {
    def apply(a: Any, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) Right(()) else Left(AssertionError.failure("never"))
  }

  sealed trait Regex { self =>
    import Regex._

    def ~(that: Regex): Regex = AndThen(self, that)

    def |(that: Regex): Regex = OrElse(self, that)

    def * : Regex = min(0)

    def + : Regex = min(1)

    def ? : Regex = between(0, 1)

    def between(min: Int, max: Int): Regex =
      Repeat(self, Some(min), Some(max))

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
    val nonAlphanumeric: Regex = Alphanumeric(reversed = true)
    val whitespace: Regex      = Whitespace(reversed = false)
    val nonWhitespace: Regex   = Whitespace(reversed = true)
    val digit: Regex           = Digit(reversed = false)
    val nonDigit: Regex        = Digit(reversed = true)
    val start: Regex           = Start
    val end: Regex             = End

    def literal(str: String): Regex =
      str.toList.foldLeft(anything)((acc, char) => acc ~ Literal(char))

    def anyCharOf(first: Char, second: Char, rest: Char*): Regex =
      anyRegexOf(literal(first.toString), literal(second.toString), rest.map(c => literal(c.toString)): _*)

    def anyRegexOf(first: Regex, second: Regex, rest: Regex*): Regex =
      CharacterSet(Set(first, second) ++ rest.toSet, reversed = false)

    def notAnyCharOf(first: Char, second: Char, rest: Char*): Regex =
      notAnyRegexOf(literal(first.toString), literal(second.toString), rest.map(c => literal(c.toString)): _*)

    def notAnyRegexOf(first: Regex, second: Regex, rest: Regex*): Regex =
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

    final case class CharacterSet(set: Set[Regex], reversed: Boolean) extends Regex {
      def compile: String = set.map(_.compile).mkString(if (reversed) "[^" else "[", "", "]")
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
          case (_, Some(0), Some(1))     => raw"(${regex.compile})?"
          case (_, Some(min), Some(max)) => raw"(${regex.compile}){$min,$max}"
          case (_, Some(0), None)        => raw"(${regex.compile})*"
          case (_, Some(1), None)        => raw"(${regex.compile})+"
          case (_, Some(min), None)      => raw"(${regex.compile}){$min,}"
          case (_, None, Some(max))      => raw"(${regex.compile}){0,$max}"
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
