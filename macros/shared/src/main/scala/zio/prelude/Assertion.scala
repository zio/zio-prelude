package zio.prelude

import scala.util.matching

/**
 * A check on values of type `A` that reports which conditions a value failed.
 * Applying it returns `Right(())` when the value passes, or a `Left` with an
 * `AssertionError` that names each failed condition.
 *
 * Assertions are built with the constructors in the companion object and
 * combined with `&&`, `||` and `!`.
 */
sealed trait Assertion[-A] { self =>
  import Assertion._

  /**
   * Returns an assertion that passes when both this assertion and `that`
   * pass. When both fail, the error lists both failures.
   */
  def &&[A1 <: A](that: Assertion[A1]): Assertion[A1] = And(self, that)

  /**
   * Returns an assertion that passes when this assertion or `that` passes.
   * When both fail, the error lists both failures.
   */
  def ||[A1 <: A](that: Assertion[A1]): Assertion[A1] = Or(self, that)

  /**
   * Returns the negation of this assertion. Its failure message names the
   * opposite condition: `!greaterThan(5)` applied to 7 fails with
   * `lessThanOrEqualTo(5)`.
   *
   * Negating `divisibleBy` or `powerOf` keeps their pass/fail result and only
   * changes the message: `!divisibleBy(3)` accepts 9 and rejects 10 with
   * `notDivisibleBy(3)`.
   */
  def unary_! : Assertion[A] = Not(self)

  /**
   * Checks `a`, returning `Right(())` when it passes, or a `Left` with the
   * failed conditions.
   */
  def apply(a: A): Either[AssertionError, Unit] = self.apply(a, negated = false)

  protected def apply(a: A, negated: Boolean): Either[AssertionError, Unit]
}

/**
 * An `Assertion[A]` is a composable predicate on `A` that reports which
 * conditions a value failed. Assertions combine with `&&`, `||` and `!`.
 * Their main use is in `Newtype` and `Subtype`: there an assertion checks
 * literal values at compile time and other values at run time through `make`.
 *
 * An assertion can be applied to a value directly:
 *
 * {{{
 * val pin: Assertion[Int] = Assertion.between(1000, 9999)
 *
 * pin(5412)
 * // Right(())
 * pin(234)
 * // Left(Failure(between(1000, 9999)))
 * }}}
 *
 * A refined newtype for 4-digit numbers on Scala 2:
 *
 * {{{
 * // scala-2-only
 * type Pin = Pin.Type
 * object Pin extends Newtype[Int] {
 *   override def assertion = assert(Assertion.between(1000, 9999))
 * }
 *
 * val ok = Pin(5412)
 * val checked = Pin.make(234)
 * // Failure(Chunk(),NonEmptyChunk(234 did not satisfy between(1000, 9999)))
 *
 * // Pin(34567) does not compile: "34567 did not satisfy between(1000, 9999)"
 * }}}
 *
 * The same newtype on Scala 3:
 *
 * {{{
 * // scala-3-only
 * type Pin = Pin.Type
 * object Pin extends Newtype[Int] {
 *   override inline def assertion = Assertion.between(1000, 9999)
 * }
 *
 * val ok = Pin(5412)
 * val checked = Pin.make(234)
 * // Failure(Chunk(),NonEmptyChunk(234 did not satisfy between(1000, 9999)))
 *
 * // Pin(34567) does not compile: "34567 did not satisfy between(1000, 9999)"
 * }}}
 */
object Assertion {

  /** An assertion that every value passes. */
  val anything: Assertion[Any] = Assertion.Anything

  /**
   * Ensures the value falls between a given min and max (inclusive), using
   * the given `Ordering`.
   */
  def between[A](min: A, max: A)(implicit ordering: Ordering[A]): Assertion[A] = Between(min, max)

  /**
   * Checks that the value is a multiple of `n`. Both numbers are converted to
   * `Double` and the check is `value % n == 0`, so `divisibleBy(0.1)` rejects
   * `0.3`.
   *
   * Negating it keeps the same pass/fail result: `!divisibleBy(3)` accepts 9
   * and rejects 10 with `notDivisibleBy(3)`.
   */
  def divisibleBy[A](n: A)(implicit numeric: Numeric[A]): Assertion[A] = DivisibleBy(n)

  /** Checks that the string contains `string`. */
  def contains(string: String): Assertion[String] = Contains(string)

  /** Checks that the value is equal to `value`, using `==`. */
  def equalTo[A](value: A): Assertion[A] = EqualTo(value)

  /** Checks that the string ends with `suffix`. */
  def endsWith(suffix: String): Assertion[String] = EndsWith(suffix)

  /**
   * Checks that the string ends with `suffix`, ignoring case. Both strings are
   * compared after `toLowerCase`.
   */
  def endsWithIgnoreCase(suffix: String): Assertion[String] = EndsWithIgnoreCase(suffix)

  /** Checks that the value is greater than `value`, using the given `Ordering`. */
  def greaterThan[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = GreaterThan(value)

  /** Checks that the value is greater than or equal to `value`, using the given `Ordering`. */
  def greaterThanOrEqualTo[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = !lessThan(value)

  /**
   * Checks the length of a string with `lengthAssertion`, for example
   * `hasLength(between(1, 10))`.
   */
  def hasLength(lengthAssertion: Assertion[Int]): Assertion[String] = HasLength(lengthAssertion)

  /** Checks that the string is empty. */
  val isEmptyString: Assertion[String] = hasLength(equalTo(0))

  /** Checks that the value is less than `value`, using the given `Ordering`. */
  def lessThan[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = LessThan(value)

  /** Checks that the value is less than or equal to `value`, using the given `Ordering`. */
  def lessThanOrEqualTo[A](value: A)(implicit ordering: Ordering[A]): Assertion[A] = !greaterThan(value)

  /**
   * Checks that the whole string matches `regex`, a pattern built with the
   * `Assertion.Regex` constructors.
   */
  def matches(regex: Regex): Assertion[String] = Matches(regex.compile)

  /**
   * Checks that the whole string matches the regular expression
   * `regexString`, as `String.matches` does.
   */
  def matches(regexString: String): Assertion[String] = Matches(regexString)

  /**
   * Matches a [[scala.util.matching.Regex]]. The whole string must match.
   *
   * In order to use this for compile-time Assertions, make sure to use the
   * string literal extension method, e.g.:
   *
   * {{{
   *   Assertion.matches("\\w+@\\d{3,5}".r)
   * }}}
   */
  def matches(regex: matching.Regex): Assertion[String] = Matches(regex.regex)

  /** An assertion that every value fails, with the message `never`. */
  val never: Assertion[Any] = !anything

  /** Checks that the value is not equal to `value`, using `==`. */
  def notEqualTo[A](value: A): Assertion[A] = !equalTo(value)

  /**
   * Checks that the value is a power of `base`: for `powerOf(2)`, the values
   * 1, 2, 4, 8 and so on pass. Both numbers are converted to `Double` before
   * the check, so a large `Long` can pass after rounding: `powerOf(2L)`
   * accepts `Long.MaxValue`.
   *
   * Negating it keeps the same pass/fail result: `!powerOf(2)` accepts 8 and
   * rejects 9 with `notPowerOf(2)`.
   *
   * {{{
   * // scala-2-only
   * type PowerOfTwo = PowerOfTwo.Type
   * object PowerOfTwo extends Newtype[Int] {
   *   override def assertion =
   *     assert(Assertion.powerOf(2))
   * }
   *
   * val ok = PowerOfTwo(1024)
   * // PowerOfTwo(1025) does not compile: "1025 did not satisfy powerOf(2)"
   * }}}
   */
  def powerOf[A](base: A)(implicit numeric: Numeric[A]): Assertion[A] = PowerOf(base)

  /** Checks that the string starts with `prefix`. */
  def startsWith(prefix: String): Assertion[String] = StartsWith(prefix)

  /**
   * Checks that the string starts with `prefix`, ignoring case. Both strings
   * are compared after `toLowerCase`.
   */
  def startsWithIgnoreCase(prefix: String): Assertion[String] = StartsWithIgnoreCase(prefix)

  private[prelude] final case class And[A](left: Assertion[A], right: Assertion[A]) extends Assertion[A] {
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

  private[prelude] final case class Or[A](left: Assertion[A], right: Assertion[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        (left.apply(a, negated), right.apply(a, negated)) match {
          case (Left(e1), Left(e2)) => Left(e1 ++ e2)
          case _                    => Right(())
        }
      } else (!left && !right).apply(a, negated = false)
  }

  private[prelude] final case class Not[A](assertion: Assertion[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      assertion.apply(a, !negated)
  }

  private[prelude] final case class DivisibleBy[A](n: A)(implicit numeric: Numeric[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] = {
      val result = numeric.toDouble(a) % numeric.toDouble(n) == 0
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.Failure(s"divisibleBy($n)"))
      } else {
        if (result) Left(AssertionError.Failure(s"notDivisibleBy($n)"))
        else Right(())
      }
    }
  }

  private[prelude] final case class Contains(string: String) extends Assertion[String] {
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

  private[prelude] final case class EndsWith(suffix: String) extends Assertion[String] {
    def apply(a: String, negated: Boolean): Either[AssertionError, Unit] = {
      val result = a.endsWith(suffix)
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.Failure(s"endsWith($suffix)"))
      } else {
        if (result) Left(AssertionError.Failure(s"doesNotEndsWith($suffix)"))
        else Right(())
      }
    }
  }

  private[prelude] final case class EndsWithIgnoreCase(suffix: String) extends Assertion[String] {
    private val loweredPrefix: String = suffix.toLowerCase

    def apply(a: String, negated: Boolean): Either[AssertionError, Unit] = {
      val result = a.toLowerCase.endsWith(loweredPrefix)
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.Failure(s"endsWithIgnoreCase($suffix)"))
      } else {
        if (result) Left(AssertionError.Failure(s"doesNotEndsWithIgnoreCase($suffix)"))
        else Right(())
      }
    }
  }

  private[prelude] final case class EqualTo[A](value: A) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        if (a == value) Right(())
        else Left(AssertionError.failure(s"equalTo($value)"))
      } else {
        if (a != value) Right(())
        else Left(AssertionError.failure(s"notEqualTo($value)"))
      }
  }

  private[prelude] final case class Between[A](min: A, max: A)(implicit ordering: Ordering[A]) extends Assertion[A] {
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

  private[prelude] final case class GreaterThan[A](value: A)(implicit ordering: Ordering[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        if (ordering.gt(a, value)) Right(())
        else Left(AssertionError.failure(s"greaterThan($value)"))
      } else {
        if (ordering.lteq(a, value)) Right(())
        else Left(AssertionError.failure(s"lessThanOrEqualTo($value)"))
      }
  }

  private[prelude] final case class HasLength[A](lengthAssertion: Assertion[Int]) extends Assertion[String] {
    def apply(string: String, negated: Boolean): Either[AssertionError, Unit] =
      lengthAssertion(string.length, negated) match {
        case Left(AssertionError.Failure(condition)) => Left(AssertionError.failure(s"hasLength($condition)"))
        case other                                   => other
      }
  }

  private[prelude] final case class LessThan[A](value: A)(implicit ordering: Ordering[A]) extends Assertion[A] {
    def apply(a: A, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) {
        if (ordering.lt(a, value)) Right(())
        else Left(AssertionError.failure(s"lessThan($value)"))
      } else {
        if (ordering.gteq(a, value)) Right(())
        else Left(AssertionError.failure(s"greaterThanOrEqualTo($value)"))
      }
  }

  private[prelude] final case class Matches(regexString: String) extends Assertion[String] {
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

  private[prelude] final case class PowerOf[A](base: A)(implicit numeric: Numeric[A]) extends Assertion[A] {
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

  private[prelude] final case class StartsWith(prefix: String) extends Assertion[String] {
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

  private[prelude] final case class StartsWithIgnoreCase(prefix: String) extends Assertion[String] {
    private val loweredPrefix: String = prefix.toLowerCase

    def apply(a: String, negated: Boolean): Either[AssertionError, Unit] = {
      val result = a.toLowerCase.startsWith(loweredPrefix)
      if (!negated) {
        if (result) Right(())
        else Left(AssertionError.Failure(s"startsWithIgnoreCase($prefix)"))
      } else {
        if (result) Left(AssertionError.Failure(s"doesNotStartWithIgnoreCase($prefix)"))
        else Right(())
      }
    }
  }

  private[prelude] object Anything extends Assertion[Any] {
    def apply(a: Any, negated: Boolean): Either[AssertionError, Unit] =
      if (!negated) Right(()) else Left(AssertionError.failure("never"))
  }

  /**
   * A regular expression built from parts, for use with `Assertion.matches`.
   * The parts come from the constructors in the `Regex` companion and combine
   * with `~` (sequence), `|` (alternative) and the repetition operators.
   * `compile` turns the result into a Java regular expression string.
   *
   * {{{
   * import Assertion.Regex._
   *
   * val zip = digit.between(5, 5) ~ (literal("-") ~ digit.between(4, 4)).?
   *
   * zip.compile
   * // (\d){5,5}(-(\d){4,4})?
   * Assertion.matches(zip).apply("12345-6789")
   * // Right(())
   * }}}
   */
  sealed trait Regex { self =>
    import Regex._

    /**
     * Matches this regex followed by `that`. When either side is
     * `Regex.anything`, it compiles to the other side alone.
     */
    def ~(that: Regex): Regex = AndThen(self, that)

    /**
     * Matches this regex or `that`. When either side is `Regex.anything`, it
     * compiles to the other side alone: `digit | anything` compiles to `\d`.
     */
    def |(that: Regex): Regex = OrElse(self, that)

    /** Repeats this regex zero or more times; the same as `min(0)`. */
    def * : Regex = min(0)

    /**
     * Repeats this regex one or more times; the same as `min(1)`.
     *
     * On Scala 2, the compile-time check of a newtype's `assert(...)` reads
     * `r.+` as `r.*`, so a literal that repeats `r` zero times passes it;
     * `make` checks the value with `r.+`.
     */
    def + : Regex = min(1)

    /** Matches this regex zero or one time; the same as `between(0, 1)`. */
    def ? : Regex = between(0, 1)

    /** Repeats this regex from `min` to `max` times, both inclusive. */
    def between(min: Int, max: Int): Regex =
      Repeat(self, Some(min), Some(max))

    /**
     * Repeats this regex at least `n` times. On a regex that is already a
     * repetition, it keeps the maximum: `digit.max(5).min(2)` compiles to
     * `(\d){2,5}`.
     */
    def min(n: Int): Regex =
      self match {
        case Repeat(regex, _, max) => Repeat(regex, Some(n), max)
        case regex                 => Repeat(regex, Some(n), None)
      }

    /**
     * Repeats this regex at most `n` times. On a regex that is already a
     * repetition, it keeps the minimum: `digit.min(2).max(5)` compiles to
     * `(\d){2,5}`.
     */
    def max(n: Int): Regex =
      self match {
        case Repeat(regex, min, _) => Repeat(regex, min, Some(n))
        case regex                 => Repeat(regex, None, Some(n))
      }

    /** Returns this regex as a Java regular expression string. */
    def compile: String
  }

  /** Constructors for `Regex` values. */
  object Regex {

    /** One character other than a line terminator; compiles to `.`. */
    val anyChar: Regex = AnyChar

    /** A word character; compiles to `\w`. */
    val alphanumeric: Regex = Alphanumeric(reversed = false)

    /** The empty regex; compiles to the empty string. */
    val anything: Regex = Anything

    /** A character that is not a word character; compiles to `\W`. */
    val nonAlphanumeric: Regex = Alphanumeric(reversed = true)

    /** A whitespace character; compiles to `\s`. */
    val whitespace: Regex = Whitespace(reversed = false)

    /** A character that is not whitespace; compiles to `\S`. */
    val nonWhitespace: Regex = Whitespace(reversed = true)

    /** A digit; compiles to `\d`. */
    val digit: Regex = Digit(reversed = false)

    /** A character that is not a digit; compiles to `\D`. */
    val nonDigit: Regex = Digit(reversed = true)

    /** The start of the input; compiles to `^`. */
    val start: Regex = Start

    /** The end of the input; compiles to `\$`. */
    val end: Regex = End

    /**
     * A regex made of the characters of `str`, one `Literal` per character, in
     * order. An empty `str` gives `anything`.
     */
    def literal(str: String): Regex =
      str.toList.foldLeft(anything)((acc, char) => acc ~ Literal(char))

    /**
     * One character out of the given characters; `anyCharOf('a', 'b', 'c')`
     * compiles to `[abc]`.
     */
    def anyCharOf(first: Char, second: Char, rest: Char*): Regex =
      anyRegexOf(literal(first.toString), literal(second.toString), rest.map(c => literal(c.toString)): _*)

    /**
     * Places the given regexes in one character class:
     * `anyRegexOf(digit, inRange('a', 'f'))` compiles to `[\d[a-f]]`.
     */
    def anyRegexOf(first: Regex, second: Regex, rest: Regex*): Regex =
      CharacterSet(Set(first, second) ++ rest.toSet, reversed = false)

    /**
     * One character that is none of the given characters;
     * `notAnyCharOf('a', 'b')` compiles to `[^ab]`.
     */
    def notAnyCharOf(first: Char, second: Char, rest: Char*): Regex =
      notAnyRegexOf(literal(first.toString), literal(second.toString), rest.map(c => literal(c.toString)): _*)

    /**
     * Places the given regexes in one negated character class:
     * `notAnyRegexOf(digit, whitespace)` compiles to `[^\d\s]`.
     */
    def notAnyRegexOf(first: Regex, second: Regex, rest: Regex*): Regex =
      CharacterSet(Set(first, second) ++ rest.toSet, reversed = true)

    /** One character from `start` to `end`, both inclusive; compiles to `[start-end]`. */
    def inRange(start: Char, end: Char): Regex = Range(start, end, reversed = false)

    /** One character outside `start` to `end`; compiles to `[^start-end]`. */
    def notInRange(start: Char, end: Char): Regex = Range(start, end, reversed = true)

    /** The regex behind `anyChar`. */
    case object AnyChar extends Regex {

      /** Returns `"."`. */
      def compile: String = "."
    }

    /** The regex behind `end`. */
    case object End extends Regex {

      /** Returns `"\$"`. */
      def compile: String = "$"
    }

    /** The regex behind `anything`. */
    case object Anything extends Regex {

      /** Returns the empty string. */
      def compile: String = ""
    }

    /** The regex behind `alphanumeric` and, when `reversed`, `nonAlphanumeric`. */
    final case class Alphanumeric(reversed: Boolean) extends Regex {

      /** Returns `\W` when `reversed`, otherwise `\w`. */
      def compile: String = if (reversed) raw"\W" else raw"\w"
    }

    /** The regex behind `whitespace` and, when `reversed`, `nonWhitespace`. */
    final case class Whitespace(reversed: Boolean) extends Regex {

      /** Returns `\S` when `reversed`, otherwise `\s`. */
      def compile: String = if (reversed) raw"\S" else raw"\s"
    }

    /** The regex behind `digit` and, when `reversed`, `nonDigit`. */
    final case class Digit(reversed: Boolean) extends Regex {

      /** Returns `\D` when `reversed`, otherwise `\d`. */
      def compile: String = if (reversed) raw"\D" else raw"\d"
    }

    /** One character of a `literal`. */
    final case class Literal(char: Char) extends Regex {

      /** Returns `char` as a one-character string. */
      def compile: String = s"$char"
    }

    /** The regex behind `anyRegexOf`, `anyCharOf` and their negated forms. */
    final case class CharacterSet(set: Set[Regex], reversed: Boolean) extends Regex {

      /** Returns the compiled members inside `[...]`, or inside `[^...]` when `reversed`. */
      def compile: String = set.map(_.compile).mkString(if (reversed) "[^" else "[", "", "]")
    }

    /** The regex behind `inRange` and, when `reversed`, `notInRange`. */
    final case class Range(start: Char, end: Char, reversed: Boolean) extends Regex {

      /** Returns `[start-end]`, or `[^start-end]` when `reversed`. */
      def compile: String = s"${if (reversed) "[^" else "["}$start-$end]"
    }

    /** The regex behind `start`. */
    case object Start extends Regex {

      /** Returns `"^"`. */
      def compile: String = "^"
    }

    /**
     * `regex` repeated between `min` and `max` times; `None` leaves that bound
     * open.
     */
    final case class Repeat(regex: Regex, min: Option[Int], max: Option[Int]) extends Regex {

      /**
       * Returns the compiled `regex` in parentheses followed by `?`, `*`, `+`
       * or a `{min,max}` quantifier, chosen from the bounds. Returns `regex`
       * alone when both bounds are `None`, and the empty string when `regex`
       * is `Anything`.
       */
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

    /** The regex behind `~`: `first` followed by `second`. */
    final case class AndThen(first: Regex, second: Regex) extends Regex {

      /**
       * Returns the compiled `first` followed by the compiled `second`. When
       * either side is `Anything`, returns the other side alone.
       */
      def compile: String =
        (first, second) match {
          case (first, Anything)  => first.compile
          case (Anything, second) => second.compile
          case _                  => raw"${first.compile}${second.compile}"
        }
    }

    /** The regex behind `|`: `first` or `second`. */
    final case class OrElse(first: Regex, second: Regex) extends Regex {

      /**
       * Returns `(first|second)` with both sides compiled. When either side is
       * `Anything`, returns the other side alone.
       */
      def compile: String =
        (first, second) match {
          case (first, Anything)  => first.compile
          case (Anything, second) => second.compile
          case _                  => raw"(${first.compile}|${second.compile})"
        }
    }
  }
}
