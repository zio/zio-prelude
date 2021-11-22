package zio.prelude

import com.github.ghik.silencer.silent
import zio.prelude.Assertion.Regex

import scala.reflect.macros.whitebox

// Wrongly emits warnings on Scala 2.12.x https://github.com/scala/bug/issues/11918
@silent("pattern var .* in method unapply is never used: use a wildcard `_` or suppress this warning with .*")
trait Liftables {
  val c: whitebox.Context

  import c.universe._

  final val AssertionPrefix: Select = q"_root_.zio.prelude.Assertion"

  implicit def optionUnliftable[T](implicit u: Unliftable[T]): Unliftable[Option[T]] = Unliftable[Option[T]] {
    case q"scala.None"               => None
    case q"scala.Some.apply[$_]($v)" => Some(u.unapply(v).get)
  }

  implicit lazy val regexLiftable: Liftable[Assertion.Regex] =
    Liftable[Assertion.Regex] {
      case Assertion.Regex.AnyChar                     => q"$AssertionPrefix.Regex.AnyChar"
      case Assertion.Regex.Anything                    => q"$AssertionPrefix.Regex.Anything"
      case Assertion.Regex.End                         => q"$AssertionPrefix.Regex.End"
      case Assertion.Regex.Alphanumeric(reversed)      => q"$AssertionPrefix.Regex.Alphanumeric($reversed)"
      case Assertion.Regex.Whitespace(reversed)        => q"$AssertionPrefix.Regex.Whitespace($reversed)"
      case Assertion.Regex.Digit(reversed)             => q"$AssertionPrefix.Regex.Digit($reversed)"
      case Assertion.Regex.Literal(char)               => q"$AssertionPrefix.Regex.Literal($char)"
      case Assertion.Regex.CharacterSet(set, reversed) =>
        q"$AssertionPrefix.Regex.CharacterSet(Set(..$set), $reversed)"
      case Assertion.Regex.Range(start, end, reversed) => q"$AssertionPrefix.Regex.Range($start, $end, $reversed)"
      case Assertion.Regex.Start                       => q"$AssertionPrefix.Regex.Start"
      case Assertion.Regex.Repeat(regex, min, max)     => q"$AssertionPrefix.Regex.Repeat($regex, $min, $max)"
      case Assertion.Regex.AndThen(first, second)      => q"$AssertionPrefix.Regex.AndThen($first, $second)"
      case Assertion.Regex.OrElse(first, second)       => q"$AssertionPrefix.Regex.OrElse($first, $second)"
    }

  implicit lazy val regexUnliftable: Unliftable[Assertion.Regex] =
    Unliftable[Assertion.Regex] {
      case q"${A(_)}.Regex.AnyChar"                                                                           =>
        Assertion.Regex.AnyChar
      case q"${A(_)}.Regex.anyChar"                                                                           =>
        Assertion.Regex.anyChar
      case q"${A(_)}.Regex.Anything"                                                                          =>
        Assertion.Regex.Anything
      case q"${A(_)}.Regex.anything"                                                                          =>
        Assertion.Regex.anything
      case q"${A(_)}.Regex.Alphanumeric.apply(${reversed: Boolean})"                                          =>
        Assertion.Regex.Alphanumeric(reversed)
      case q"${A(_)}.Regex.alphanumeric"                                                                      =>
        Assertion.Regex.alphanumeric
      case q"${A(_)}.Regex.nonAlphanumeric"                                                                   =>
        Assertion.Regex.nonAlphanumeric
      case q"${A(_)}.Regex.Whitespace.apply(${reversed: Boolean})"                                            =>
        Assertion.Regex.Whitespace(reversed)
      case q"${A(_)}.Regex.whitespace"                                                                        =>
        Assertion.Regex.whitespace
      case q"${A(_)}.Regex.nonWhitespace"                                                                     =>
        Assertion.Regex.nonWhitespace
      case q"${A(_)}.Regex.Digit.apply(${reversed: Boolean})"                                                 =>
        Assertion.Regex.Digit(reversed)
      case q"${A(_)}.Regex.digit"                                                                             =>
        Assertion.Regex.digit
      case q"${A(_)}.Regex.nonDigit"                                                                          =>
        Assertion.Regex.nonDigit
      case q"${A(_)}.Regex.Literal.apply(${char: Char})"                                                      =>
        Assertion.Regex.Literal(char)
      case q"${A(_)}.Regex.literal(${str: String})"                                                           =>
        Assertion.Regex.literal(str)
      case q"${A(_)}.Regex.CharacterSet.apply(scala.Predef.Set.apply[$_](..$set), ${reversed: Boolean})"      =>
        Assertion.Regex.CharacterSet(set.map(t => c.eval[Regex](c.Expr[Regex](t))).toSet, reversed)
      case q"${A(_)}.Regex.anyCharOf(${first: Char}, ${second: Char})"                                        =>
        Assertion.Regex.anyCharOf(first, second)
      case q"${A(_)}.Regex.anyCharOf(${first: Char}, ${second: Char}, ..$rest)"                               =>
        Assertion.Regex.anyCharOf(first, second, rest.map(t => c.eval[Char](c.Expr[Char](t))): _*)
      case q"${A(_)}.Regex.anyRegexOf(${first: Regex}, ${second: Regex})"                                     =>
        Assertion.Regex.anyRegexOf(first, second)
      case q"${A(_)}.Regex.anyRegexOf(${first: Regex}, ${second: Regex}, ..$rest)"                            =>
        Assertion.Regex.anyRegexOf(first, second, unliftRegexes(rest): _*)
      case q"${A(_)}.Regex.notAnyCharOf(${first: Char}, ${second: Char})"                                     =>
        Assertion.Regex.notAnyCharOf(first, second)
      case q"${A(_)}.Regex.notAnyCharOf(${first: Char}, ${second: Char}, ..$rest)"                            =>
        Assertion.Regex.notAnyCharOf(first, second, rest.map(t => c.eval[Char](c.Expr[Char](t))): _*)
      case q"${A(_)}.Regex.notAnyRegexOf(${first: Regex}, ${second: Regex})"                                  =>
        Assertion.Regex.notAnyRegexOf(first, second)
      case q"${A(_)}.Regex.notAnyRegexOf(${first: Regex}, ${second: Regex}, ..$rest)"                         =>
        Assertion.Regex.notAnyRegexOf(first, second, unliftRegexes(rest): _*)
      case q"${A(_)}.Regex.Range.apply(${start: Char}, ${end: Char}, ${reversed: Boolean})"                   =>
        Assertion.Regex.Range(start, end, reversed)
      case q"${A(_)}.Regex.inRange(${start: Char}, ${end: Char})"                                             =>
        Assertion.Regex.inRange(start, end)
      case q"${A(_)}.Regex.notInRange(${start: Char}, ${end: Char})"                                          =>
        Assertion.Regex.notInRange(start, end)
      case q"${A(_)}.Regex.start"                                                                             =>
        Assertion.Regex.start
      case q"${A(_)}.Regex.end"                                                                               =>
        Assertion.Regex.end
      case q"${A(_)}.Regex.Repeat.apply(${regex: Assertion.Regex}, ${min: Option[Int]}, ${max: Option[Int]})" =>
        Assertion.Regex.Repeat(regex, min, max)
      case q"${regex: Assertion.Regex}.min(${n: Int})"                                                        =>
        regex.min(n)
      case q"${regex: Assertion.Regex}.max(${n: Int})"                                                        =>
        regex.max(n)
      case q"${regex: Assertion.Regex}.between(${min: Int}, ${max: Int})"                                     =>
        regex.between(min, max)
      case q"${regex: Assertion.Regex}.*"                                                                     =>
        regex.*
      case q"${regex: Assertion.Regex}.+"                                                                     =>
        regex.*
      case q"${regex: Assertion.Regex}.?"                                                                     =>
        regex.?
      case q"${A(_)}.Regex.AndThen.apply(${first: Assertion.Regex}, ${second: Assertion.Regex})"              =>
        Assertion.Regex.AndThen(first, second)
      case q"${left: Assertion.Regex}.~(${right: Assertion.Regex})"                                           =>
        left ~ right
      case q"${A(_)}.Regex.OrElse.apply(${first: Assertion.Regex}, ${second: Assertion.Regex})"               =>
        Assertion.Regex.OrElse(first, second)
      case q"${left: Assertion.Regex}.|(${right: Assertion.Regex})"                                           =>
        left | right
    }

  def unliftRegexes(ts: Seq[c.Tree]): Seq[Regex] = ts.map(t => regexUnliftable.unapply(t).get)

  // Match on zio.prelude.Assertion path prefix
  object A {
    def unapply(tree: c.Tree): Option[Assertion.type] =
      tree match {
        case q"zio.prelude.Assertion" => Some(Assertion)
        case q"Assertion"             => Some(Assertion)
        case _                        => None
      }
  }

  implicit def scalaRegexUnliftable[A: c.WeakTypeTag]: Unliftable[scala.util.matching.Regex] =
    Unliftable[scala.util.matching.Regex] {
      case q"scala.Predef.augmentString(${string: String}).r"      => string.r
      case q"scala.this.Predef.augmentString(${string: String}).r" => string.r
    }

  @silent("Implicit resolves to enclosing method")
  implicit def assertionUnliftable[A: c.WeakTypeTag]: Unliftable[Assertion[A]] =
    Unliftable[Assertion[A]] {

      case q"${A(_)}.anything" =>
        Assertion.anything

      case q"${A(_)}.never" =>
        Assertion.never

      case q"${left: Assertion[A]}.&&[$_](${right: Assertion[A]})" =>
        Assertion.And(left, right)

      case q"${A(_)}.divisibleBy[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.divisibleBy(value)(numericForValue(value))

      case q"${A(_)}.contains(${value: String})" =>
        Assertion.contains(value).asInstanceOf[Assertion[A]]

      case q"${A(_)}.endsWith(${value: String})" =>
        Assertion.endsWith(value).asInstanceOf[Assertion[A]]

      case q"${A(_)}.equalTo[$_](${LiteralUnlift(value)})" =>
        Assertion.equalTo(value.asInstanceOf[A])

      case q"${A(_)}.notEqualTo[$_](${LiteralUnlift(value)})" =>
        Assertion.notEqualTo(value.asInstanceOf[A])

      case q"${A(_)}.between[$_](${LiteralUnlift(min)}, ${LiteralUnlift(max)})($_)" =>
        Assertion.between(min.asInstanceOf[A], max.asInstanceOf[A])(orderingForValue(min).asInstanceOf[Ordering[A]])

      case q"${A(_)}.greaterThan[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.greaterThan(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"${A(_)}.greaterThanOrEqualTo[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.greaterThanOrEqualTo(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"${A(_)}.hasLength(${assertion: Assertion[Int]})" =>
        Assertion.hasLength(assertion).asInstanceOf[Assertion[A]]

      case q"${A(_)}.isEmptyString" =>
        Assertion.isEmptyString.asInstanceOf[Assertion[A]]

      case q"${A(_)}.lessThan[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.lessThan(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"${A(_)}.lessThanOrEqualTo[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.lessThanOrEqualTo(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"${A(_)}.matches(${regex: Assertion.Regex})" =>
        Assertion.Matches(regex.compile).asInstanceOf[Assertion[A]]

      case q"${A(_)}.matches(${string: String})" =>
        Assertion.Matches(string).asInstanceOf[Assertion[A]]

      case q"${A(_)}.matches(${regex: scala.util.matching.Regex})" =>
        Assertion.Matches(regex.regex).asInstanceOf[Assertion[A]]

      case q"${A(_)}.powerOf[$_](${LiteralUnlift(base)})($_)" =>
        Assertion.powerOf(base)(numericForValue(base)).asInstanceOf[Assertion[A]]

      case q"${A(_)}.startsWith(${value: String})" =>
        Assertion.startsWith(value).asInstanceOf[Assertion[A]]

      case q"!${assertion: Assertion[A]}" =>
        Assertion.Not(assertion)

      case q"${left: Assertion[A]}.||[$_](${right: Assertion[A]})" =>
        Assertion.Or(left, right)
    }

  object LiteralUnlift {
    def unapply(expr: Tree): Option[Any] = expr match {
      case q"${int: Int}"       => Some(int)
      case q"Int.MaxValue"      => Some(Int.MaxValue)
      case q"Int.MinValue"      => Some(Int.MinValue)
      case q"${string: String}" => Some(string)
      case q"${double: Double}" => Some(double)
      case q"${float: Float}"   => Some(float)
      case q"${long: Long}"     => Some(long)
      case q"${short: Short}"   => Some(short)
      case q"${byte: Byte}"     => Some(byte)
      case q"${byte: Byte}"     => Some(byte)
      case q"List(..$values)"   =>
        val results = values.flatMap(unapply(_))
        Some(results.toList)
      case _                    => None
    }
  }

  object LiteralLift {
    def unapply(any: Any): Option[Tree] = any match {
      case int: Int       => Some(q"$int")
      case string: String => Some(q"$string")
      case double: Double => Some(q"$double")
      case float: Float   => Some(q"$float")
      case long: Long     => Some(q"$long")
      case short: Short   => Some(q"$short")
      case byte: Byte     => Some(q"$byte")
      case _              => None
    }
  }

  private def orderingForValue(any: Any): Ordering[Any] = any match {
    case _: Int    => scala.Ordering.Int.asInstanceOf[Ordering[Any]]
    case _: String => scala.Ordering.String.asInstanceOf[Ordering[Any]]
    case _: Double => DoubleOrdering.asInstanceOf[Ordering[Any]]
    case _: Float  => FloatOrdering.asInstanceOf[Ordering[Any]]
    case _: Long   => scala.Ordering.Long.asInstanceOf[Ordering[Any]]
    case _: Short  => scala.Ordering.Short.asInstanceOf[Ordering[Any]]
    case _: Byte   => scala.Ordering.Byte.asInstanceOf[Ordering[Any]]
    case _: Char   => scala.Ordering.Char.asInstanceOf[Ordering[Any]]
    case other     => c.abort(c.enclosingPosition, s"NO ORDERING FOR $other")
  }

  private def numericForValue(any: Any): Numeric[Any] = any match {
    case _: Int    => scala.Numeric.IntIsIntegral.asInstanceOf[Numeric[Any]]
    case _: Double => scala.Numeric.DoubleIsFractional.asInstanceOf[Numeric[Any]]
    case _: Float  => scala.Numeric.FloatIsFractional.asInstanceOf[Numeric[Any]]
    case _: Long   => scala.Numeric.LongIsIntegral.asInstanceOf[Numeric[Any]]
    case _: Short  => scala.Numeric.ShortIsIntegral.asInstanceOf[Numeric[Any]]
    case _: Byte   => scala.Numeric.ByteIsIntegral.asInstanceOf[Numeric[Any]]
    case _: Char   => scala.Numeric.CharIsIntegral.asInstanceOf[Numeric[Any]]
    case other     => c.abort(c.enclosingPosition, s"NO ORDERING FOR $other")
  }

  /**
   * This Ordering instance exists for compatibility between 2.11, 2.12 and 2.13.
   */
  object DoubleOrdering extends Ordering[Double] {
    def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
  }

  /**
   * This Ordering instance exists for compatibility between 2.11, 2.12 and 2.13.
   */
  object FloatOrdering extends Ordering[Float] {
    def compare(x: Float, y: Float): Int = java.lang.Float.compare(x, y)
  }
}
