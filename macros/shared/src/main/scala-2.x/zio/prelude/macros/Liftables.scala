package zio.prelude.macros

import zio.prelude.refined.Assertion

import scala.reflect.macros.whitebox

trait Liftables {
  val c: whitebox.Context

  import c.universe._

  final val AssertionPrefix = q"_root_.zio.prelude.refined.Assertion"

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
      case Assertion.Regex.CharacterSet(set, reversed) => q"$AssertionPrefix.Regex.CharacterSet(Set(..$set), $reversed)"
      case Assertion.Regex.Range(start, end, reversed) => q"$AssertionPrefix.Regex.Range($start, $end, $reversed)"
      case Assertion.Regex.Start                       => q"$AssertionPrefix.Regex.Start"
      case Assertion.Regex.Repeat(regex, min, max)     => q"$AssertionPrefix.Regex.Repeat($regex, $min, $max)"
      case Assertion.Regex.AndThen(first, second)      => q"$AssertionPrefix.Regex.AndThen($first, $second)"
      case Assertion.Regex.OrElse(first, second)       => q"$AssertionPrefix.Regex.OrElse($first, $second)"
    }

  implicit lazy val regexUnliftable: Unliftable[Assertion.Regex] =
    Unliftable[Assertion.Regex] {
      case q"zio.prelude.refined.Assertion.Regex.AnyChar"                                                                           =>
        Assertion.Regex.AnyChar
      case q"zio.prelude.refined.Assertion.Regex.anyChar"                                                                           =>
        Assertion.Regex.anyChar
      case q"zio.prelude.refined.Assertion.Regex.Anything"                                                                          =>
        Assertion.Regex.Anything
      case q"zio.prelude.refined.Assertion.Regex.anything"                                                                          =>
        Assertion.Regex.anything
      case q"zio.prelude.refined.Assertion.Regex.End"                                                                               =>
        Assertion.Regex.End
      case q"zio.prelude.refined.Assertion.Regex.end"                                                                               =>
        Assertion.Regex.end
      case q"zio.prelude.refined.Assertion.Regex.Alphanumeric.apply(${reversed: Boolean})"                                          =>
        Assertion.Regex.Alphanumeric(reversed)
      case q"zio.prelude.refined.Assertion.Regex.alphanumeric"                                                                      =>
        Assertion.Regex.alphanumeric
      case q"zio.prelude.refined.Assertion.Regex.nonAlphanumeric"                                                                   =>
        Assertion.Regex.nonAlphanumeric
      case q"zio.prelude.refined.Assertion.Regex.Whitespace.apply(${reversed: Boolean})"                                            =>
        Assertion.Regex.Whitespace(reversed)
      case q"zio.prelude.refined.Assertion.Regex.whitespace"                                                                        =>
        Assertion.Regex.whitespace
      case q"zio.prelude.refined.Assertion.Regex.nonWhitespace"                                                                     =>
        Assertion.Regex.nonWhitespace
      case q"zio.prelude.refined.Assertion.Regex.Digit.apply(${reversed: Boolean})"                                                 =>
        Assertion.Regex.Digit(reversed)
      case q"zio.prelude.refined.Assertion.Regex.digit"                                                                             =>
        Assertion.Regex.digit
      case q"zio.prelude.refined.Assertion.Regex.nonDigit"                                                                          =>
        Assertion.Regex.nonDigit
      case q"zio.prelude.refined.Assertion.Regex.Literal.apply(${char: Char})"                                                      =>
        Assertion.Regex.Literal(char)
      case q"zio.prelude.refined.Assertion.Regex.literal(${str: String})"                                                           =>
        Assertion.Regex.literal(str)
      case q"zio.prelude.refined.Assertion.Regex.CharacterSet.apply(scala.Predef.Set.apply[$_](..$set), ${reversed: Boolean})"      =>
        Assertion.Regex.CharacterSet(set.map(t => c.eval[Char](c.Expr[Char](t))).toSet, reversed)
      case q"zio.prelude.refined.Assertion.Regex.anyOf(${first: Char}, ${second: Char}, ..$rest)"                                   =>
        Assertion.Regex.anyOf(first, second, rest.map(t => c.eval[Char](c.Expr[Char](t))): _*)
      case q"zio.prelude.refined.Assertion.Regex.notAnyOf(${first: Char}, ${second: Char}, ..$rest)"                                =>
        Assertion.Regex.notAnyOf(first, second, rest.map(t => c.eval[Char](c.Expr[Char](t))): _*)
      case q"zio.prelude.refined.Assertion.Regex.Range.apply(${start: Char}, ${end: Char}, ${reversed: Boolean})"                   =>
        Assertion.Regex.Range(start, end, reversed)
      case q"zio.prelude.refined.Assertion.Regex.inRange(${start: Char}, ${end: Char})"                                             =>
        Assertion.Regex.inRange(start, end)
      case q"zio.prelude.refined.Assertion.Regex.notInRange(${start: Char}, ${end: Char})"                                          =>
        Assertion.Regex.notInRange(start, end)
      case q"zio.prelude.refined.Assertion.Regex.Start"                                                                             =>
        Assertion.Regex.Start
      case q"zio.prelude.refined.Assertion.Regex.start"                                                                             =>
        Assertion.Regex.start
      case q"zio.prelude.refined.Assertion.Regex.Repeat.apply(${regex: Assertion.Regex}, ${min: Option[Int]}, ${max: Option[Int]})" =>
        Assertion.Regex.Repeat(regex, min, max)
      case q"${regex: Assertion.Regex}.min(${n: Int})"                                                                              =>
        regex.min(n)
      case q"${regex: Assertion.Regex}.max(${n: Int})"                                                                              =>
        regex.max(n)
      case q"zio.prelude.refined.Assertion.Regex.AndThen.apply(${first: Assertion.Regex}, ${second: Assertion.Regex})"              =>
        Assertion.Regex.AndThen(first, second)
      case q"${left: Assertion.Regex}.~(${right: Assertion.Regex})"                                                                 =>
        left ~ right
      case q"zio.prelude.refined.Assertion.Regex.OrElse.apply(${first: Assertion.Regex}, ${second: Assertion.Regex})"               =>
        Assertion.Regex.OrElse(first, second)
      case q"${left: Assertion.Regex}.|(${right: Assertion.Regex})"                                                                 =>
        left | right
    }

  implicit def assertionLiftable[A: c.WeakTypeTag]: Liftable[Assertion[A]] =
    Liftable[Assertion[A]] {
      case Assertion.Always                          => q"$AssertionPrefix.Always"
      case Assertion.And(left, right)                => q"$AssertionPrefix.And($left, $right)"
      case Assertion.EqualTo(LiteralLift(value))     => q"$AssertionPrefix.EqualTo($value)"
      case Assertion.GreaterThan(LiteralLift(value)) => q"$AssertionPrefix.GreaterThan($value)"
      case Assertion.LessThan(LiteralLift(value))    => q"$AssertionPrefix.LessThan($value)"
      case Assertion.Matches(regex)                  => q"$AssertionPrefix.Matches($regex)"
      case Assertion.Not(assertion)                  => q"$AssertionPrefix.Not($assertion)"
      case Assertion.Or(left, right)                 => q"$AssertionPrefix.Or($left, $right)"
    }

  implicit def assertionUnliftable[A: c.WeakTypeTag]: Unliftable[Assertion[A]] =
    Unliftable[Assertion[A]] {
      case q"zio.prelude.refined.Assertion.Always" =>
        Assertion.Always

      case q"zio.prelude.refined.Assertion.always" =>
        Assertion.always

      case q"zio.prelude.refined.Assertion.never" =>
        Assertion.never

      case q"zio.prelude.refined.Assertion.And.apply[$_](${left: Assertion[A]}, ${right: Assertion[A]})" =>
        Assertion.And(left, right)

      case q"${left: Assertion[A]}.&&[$_](${right: Assertion[A]})" =>
        Assertion.And(left, right)

      case q"zio.prelude.refined.Assertion.EqualTo.apply[$_](${LiteralUnlift(value)})" =>
        Assertion.EqualTo(value.asInstanceOf[A])

      case q"zio.prelude.refined.Assertion.equalTo[$_](${LiteralUnlift(value)})" =>
        Assertion.equalTo(value.asInstanceOf[A])

      case q"zio.prelude.refined.Assertion.notEqualTo[$_](${LiteralUnlift(value)})" =>
        Assertion.notEqualTo(value.asInstanceOf[A])

      case q"zio.prelude.refined.Assertion.GreaterThan.apply[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.GreaterThan(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"zio.prelude.refined.Assertion.greaterThan[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.greaterThan(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"zio.prelude.refined.Assertion.greaterThanOrEqualTo[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.greaterThanOrEqualTo(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"zio.prelude.refined.Assertion.LessThan.apply[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.LessThan(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"zio.prelude.refined.Assertion.lessThan[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.lessThan(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"zio.prelude.refined.Assertion.lessThanOrEqualTo[$_](${LiteralUnlift(value)})($_)" =>
        Assertion.lessThanOrEqualTo(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"zio.prelude.refined.Assertion.Matches.apply(${regex: Assertion.Regex})" =>
        Assertion.Matches(regex).asInstanceOf[Assertion[A]]

      case q"zio.prelude.refined.Assertion.matches(${regex: Assertion.Regex})" =>
        Assertion.Matches(regex).asInstanceOf[Assertion[A]]

      case q"zio.prelude.refined.Assertion.Not.apply[$_](${assertion: Assertion[A]})" =>
        Assertion.Not(assertion)

      case q"!${assertion: Assertion[A]}" =>
        Assertion.Not(assertion)

      case q"zio.prelude.refined.Assertion.Or.apply[$_](${left: Assertion[A]}, ${right: Assertion[A]})" =>
        Assertion.Or(left, right)

      case q"${left: Assertion[A]}.||[$_](${right: Assertion[A]})" =>
        Assertion.Or(left, right)
    }

  object LiteralUnlift {
    def unapply(expr: Tree): Option[Any] = expr match {
      case q"${int: Int}"       => Some(int)
      case q"${string: String}" => Some(string)
      case q"${double: Double}" => Some(double)
      case q"${float: Float}"   => Some(float)
      case q"${long: Long}"     => Some(long)
      case q"${short: Short}"   => Some(short)
      case q"${byte: Byte}"     => Some(byte)
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
    case _: Double => scala.Ordering.Double.IeeeOrdering.asInstanceOf[Ordering[Any]]
    case _: Float  => scala.Ordering.Float.IeeeOrdering.asInstanceOf[Ordering[Any]]
    case _: Long   => scala.Ordering.Long.asInstanceOf[Ordering[Any]]
    case _: Short  => scala.Ordering.Short.asInstanceOf[Ordering[Any]]
    case _: Byte   => scala.Ordering.Byte.asInstanceOf[Ordering[Any]]
    case other     => c.abort(c.enclosingPosition, s"NO ORDERING FOR $other")
  }
}
