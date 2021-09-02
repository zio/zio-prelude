package zio.prelude

import com.github.ghik.silencer.silent

import scala.reflect.macros.whitebox

trait Liftables {
  val c: whitebox.Context

  import c.universe._

  final val RefinementPrefix: Select = q"_root_.zio.prelude.Refinement"

  implicit def optionUnliftable[T](implicit u: Unliftable[T]): Unliftable[Option[T]] = Unliftable[Option[T]] {
    case q"scala.None"               => None
    case q"scala.Some.apply[$_]($v)" => Some(u.unapply(v).get)
  }

  implicit lazy val regexLiftable: Liftable[Refinement.Regex] =
    Liftable[Refinement.Regex] {
      case Refinement.Regex.AnyChar                     => q"$RefinementPrefix.Regex.AnyChar"
      case Refinement.Regex.Anything                    => q"$RefinementPrefix.Regex.Anything"
      case Refinement.Regex.End                         => q"$RefinementPrefix.Regex.End"
      case Refinement.Regex.Alphanumeric(reversed)      => q"$RefinementPrefix.Regex.Alphanumeric($reversed)"
      case Refinement.Regex.Whitespace(reversed)        => q"$RefinementPrefix.Regex.Whitespace($reversed)"
      case Refinement.Regex.Digit(reversed)             => q"$RefinementPrefix.Regex.Digit($reversed)"
      case Refinement.Regex.Literal(char)               => q"$RefinementPrefix.Regex.Literal($char)"
      case Refinement.Regex.CharacterSet(set, reversed) =>
        q"$RefinementPrefix.Regex.CharacterSet(Set(..$set), $reversed)"
      case Refinement.Regex.Range(start, end, reversed) => q"$RefinementPrefix.Regex.Range($start, $end, $reversed)"
      case Refinement.Regex.Start                       => q"$RefinementPrefix.Regex.Start"
      case Refinement.Regex.Repeat(regex, min, max)     => q"$RefinementPrefix.Regex.Repeat($regex, $min, $max)"
      case Refinement.Regex.AndThen(first, second)      => q"$RefinementPrefix.Regex.AndThen($first, $second)"
      case Refinement.Regex.OrElse(first, second)       => q"$RefinementPrefix.Regex.OrElse($first, $second)"
    }

  implicit lazy val regexUnliftable: Unliftable[Refinement.Regex] =
    Unliftable[Refinement.Regex] {
      case q"${R(_)}.Regex.AnyChar"                                                                            =>
        Refinement.Regex.AnyChar
      case q"${R(_)}.Regex.anyChar"                                                                            =>
        Refinement.Regex.anyChar
      case q"${R(_)}.Regex.Anything"                                                                           =>
        Refinement.Regex.Anything
      case q"${R(_)}.Regex.anything"                                                                           =>
        Refinement.Regex.anything
      case q"${R(_)}.Regex.End"                                                                                =>
        Refinement.Regex.End
      case q"${R(_)}.Regex.end"                                                                                =>
        Refinement.Regex.end
      case q"${R(_)}.Regex.Alphanumeric.apply(${reversed: Boolean})"                                           =>
        Refinement.Regex.Alphanumeric(reversed)
      case q"${R(_)}.Regex.alphanumeric"                                                                       =>
        Refinement.Regex.alphanumeric
      case q"${R(_)}.Regex.nonAlphanumeric"                                                                    =>
        Refinement.Regex.nonAlphanumeric
      case q"${R(_)}.Regex.Whitespace.apply(${reversed: Boolean})"                                             =>
        Refinement.Regex.Whitespace(reversed)
      case q"${R(_)}.Regex.whitespace"                                                                         =>
        Refinement.Regex.whitespace
      case q"${R(_)}.Regex.nonWhitespace"                                                                      =>
        Refinement.Regex.nonWhitespace
      case q"${R(_)}.Regex.Digit.apply(${reversed: Boolean})"                                                  =>
        Refinement.Regex.Digit(reversed)
      case q"${R(_)}.Regex.digit"                                                                              =>
        Refinement.Regex.digit
      case q"${R(_)}.Regex.nonDigit"                                                                           =>
        Refinement.Regex.nonDigit
      case q"${R(_)}.Regex.Literal.apply(${char: Char})"                                                       =>
        Refinement.Regex.Literal(char)
      case q"${R(_)}.Regex.literal(${str: String})"                                                            =>
        Refinement.Regex.literal(str)
      case q"${R(_)}.Regex.CharacterSet.apply(scala.Predef.Set.apply[$_](..$set), ${reversed: Boolean})"       =>
        Refinement.Regex.CharacterSet(set.map(t => c.eval[Char](c.Expr[Char](t))).toSet, reversed)
      case q"${R(_)}.Regex.anyOf(${first: Char}, ${second: Char}, ..$rest)"                                    =>
        Refinement.Regex.anyOf(first, second, rest.map(t => c.eval[Char](c.Expr[Char](t))): _*)
      case q"${R(_)}.Regex.notAnyOf(${first: Char}, ${second: Char}, ..$rest)"                                 =>
        Refinement.Regex.notAnyOf(first, second, rest.map(t => c.eval[Char](c.Expr[Char](t))): _*)
      case q"${R(_)}.Regex.Range.apply(${start: Char}, ${end: Char}, ${reversed: Boolean})"                    =>
        Refinement.Regex.Range(start, end, reversed)
      case q"${R(_)}.Regex.inRange(${start: Char}, ${end: Char})"                                              =>
        Refinement.Regex.inRange(start, end)
      case q"${R(_)}.Regex.notInRange(${start: Char}, ${end: Char})"                                           =>
        Refinement.Regex.notInRange(start, end)
      case q"${R(_)}.Regex.Start"                                                                              =>
        Refinement.Regex.Start
      case q"${R(_)}.Regex.start"                                                                              =>
        Refinement.Regex.start
      case q"${R(_)}.Regex.Repeat.apply(${regex: Refinement.Regex}, ${min: Option[Int]}, ${max: Option[Int]})" =>
        Refinement.Regex.Repeat(regex, min, max)
      case q"${regex: Refinement.Regex}.min(${n: Int})"                                                        =>
        regex.min(n)
      case q"${regex: Refinement.Regex}.max(${n: Int})"                                                        =>
        regex.max(n)
      case q"${R(_)}.Regex.AndThen.apply(${first: Refinement.Regex}, ${second: Refinement.Regex})"             =>
        Refinement.Regex.AndThen(first, second)
      case q"${left: Refinement.Regex}.~(${right: Refinement.Regex})"                                          =>
        left ~ right
      case q"${R(_)}.Regex.OrElse.apply(${first: Refinement.Regex}, ${second: Refinement.Regex})"              =>
        Refinement.Regex.OrElse(first, second)
      case q"${left: Refinement.Regex}.|(${right: Refinement.Regex})"                                          =>
        left | right
    }

  @silent("Implicit resolves to enclosing method")
  implicit def refinementLiftable[A: c.WeakTypeTag]: Liftable[Refinement[A]] =
    Liftable[Refinement[A]] {
      case Refinement.Always                          => q"$RefinementPrefix.Always"
      case Refinement.And(left, right)                => q"$RefinementPrefix.And($left, $right)"
      case Refinement.EqualTo(LiteralLift(value))     => q"$RefinementPrefix.EqualTo($value)"
      case Refinement.GreaterThan(LiteralLift(value)) => q"$RefinementPrefix.GreaterThan($value)"
      case Refinement.LessThan(LiteralLift(value))    => q"$RefinementPrefix.LessThan($value)"
      case Refinement.Matches(regex)                  => q"$RefinementPrefix.Matches($regex)"
      case Refinement.Not(refinement)                 => q"$RefinementPrefix.Not($refinement)"
      case Refinement.Or(left, right)                 => q"$RefinementPrefix.Or($left, $right)"
      case other                                      => c.abort(c.enclosingPosition, s"COULD NOT MATCH ASSERTION: $other")
    }

  object R {
    def unapply(tree: c.Tree): Option[Refinement.type] =
      tree match {
        case q"zio.prelude.Refinement" => Some(Refinement)
        case q"Refinement"             => Some(Refinement)
        case _                         => None
      }
  }

  @silent("Implicit resolves to enclosing method")
  implicit def refinementUnliftable[A: c.WeakTypeTag]: Unliftable[Refinement[A]] =
    Unliftable[Refinement[A]] {
      case q"${R(_)}.Always" =>
        Refinement.Always

      case q"${R(_)}.always" =>
        Refinement.always

      case q"${R(_)}.never" =>
        Refinement.never

      case q"${R(_)}.And.apply[$_](${left: Refinement[A]}, ${right: Refinement[A]})" =>
        Refinement.And(left, right)

      case q"${left: Refinement[A]}.&&[$_](${right: Refinement[A]})" =>
        Refinement.And(left, right)

      case q"${R(_)}.EqualTo.apply[$_](${LiteralUnlift(value)})" =>
        Refinement.EqualTo(value.asInstanceOf[A])

      case q"${R(_)}.equalTo[$_](${LiteralUnlift(value)})" =>
        Refinement.equalTo(value.asInstanceOf[A])

      case q"${R(_)}.notEqualTo[$_](${LiteralUnlift(value)})" =>
        Refinement.notEqualTo(value.asInstanceOf[A])

      case q"${R(_)}.GreaterThan.apply[$_](${LiteralUnlift(value)})($_)" =>
        Refinement.GreaterThan(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"${R(_)}.greaterThan[$_](${LiteralUnlift(value)})($_)" =>
        Refinement.greaterThan(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"${R(_)}.greaterThanOrEqualTo[$_](${LiteralUnlift(value)})($_)" =>
        Refinement.greaterThanOrEqualTo(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"${R(_)}.LessThan.apply[$_](${LiteralUnlift(value)})($_)" =>
        Refinement.LessThan(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"${R(_)}.lessThan[$_](${LiteralUnlift(value)})($_)" =>
        Refinement.lessThan(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"${R(_)}.lessThanOrEqualTo[$_](${LiteralUnlift(value)})($_)" =>
        Refinement.lessThanOrEqualTo(value.asInstanceOf[A])(orderingForValue(value).asInstanceOf[Ordering[A]])

      case q"${R(_)}.Matches.apply(${regex: Refinement.Regex})" =>
        Refinement.Matches(regex).asInstanceOf[Refinement[A]]

      case q"${R(_)}.matches(${regex: Refinement.Regex})" =>
        Refinement.Matches(regex).asInstanceOf[Refinement[A]]

      case q"${R(_)}.Not.apply[$_](${refinement: Refinement[A]})" =>
        Refinement.Not(refinement)

      case q"!${refinement: Refinement[A]}" =>
        Refinement.Not(refinement)

      case q"${R(_)}.Or.apply[$_](${left: Refinement[A]}, ${right: Refinement[A]})" =>
        Refinement.Or(left, right)

      case q"${left: Refinement[A]}.||[$_](${right: Refinement[A]})" =>
        Refinement.Or(left, right)
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
