package zio.prelude

import scala.quoted.*
import scala.math.{Ordering => SOrdering}

trait Liftables {
  given ToExpr[Assertion.Regex] with {
    def apply(regex: Assertion.Regex)(using Quotes): Expr[Assertion.Regex] =
      regex match {
        case Assertion.Regex.AnyChar                     => '{ Assertion.Regex.AnyChar }
        case Assertion.Regex.Anything                    => '{ Assertion.Regex.Anything }
        case Assertion.Regex.End                         => '{ Assertion.Regex.End }
        case Assertion.Regex.Alphanumeric(reversed)      => '{ Assertion.Regex.Alphanumeric(${Expr(reversed)}) }
        case Assertion.Regex.Whitespace(reversed)        => '{ Assertion.Regex.Whitespace(${Expr(reversed)}) }
        case Assertion.Regex.Digit(reversed)             => '{ Assertion.Regex.Digit(${Expr(reversed)}) }
        case Assertion.Regex.Literal(char)               => '{ Assertion.Regex.Literal(${Expr(char)}) }
        case Assertion.Regex.CharacterSet(set, reversed) => '{ Assertion.Regex.CharacterSet(${Expr(set)}, ${Expr(reversed)}) }
        case Assertion.Regex.Range(start, end, reversed) => '{ Assertion.Regex.Range(${Expr(start)}, ${Expr(end)}, ${Expr(reversed)}) }
        case Assertion.Regex.Start                       => '{ Assertion.Regex.Start }
        case Assertion.Regex.Repeat(regex, min, max)     => '{ Assertion.Regex.Repeat(${Expr(regex)}, ${Expr(min)}, ${Expr(max)}) }
        case Assertion.Regex.AndThen(first, second)      => '{ Assertion.Regex.AndThen(${Expr(first)}, ${Expr(second)}) }
        case Assertion.Regex.OrElse(first, second)       => '{ Assertion.Regex.OrElse(${Expr(first)}, ${Expr(second)}) }
      }
  }

  given FromExpr[Assertion.Regex] with {
    def unapply(assertion: Expr[Assertion.Regex])(using Quotes): Option[Assertion.Regex] = {
      import quotes.reflect.*

      assertion match {
        case '{ Assertion.Regex.anyChar }                                                              => Some(Assertion.Regex.anyChar)
        case '{ Assertion.Regex.anything }                                                             => Some(Assertion.Regex.anything)
        case '{ Assertion.Regex.alphanumeric }                                                         => Some(Assertion.Regex.alphanumeric)
        case '{ Assertion.Regex.nonAlphanumeric }                                                      => Some(Assertion.Regex.nonAlphanumeric)
        case '{ Assertion.Regex.whitespace }                                                           => Some(Assertion.Regex.whitespace)
        case '{ Assertion.Regex.nonWhitespace }                                                        => Some(Assertion.Regex.nonWhitespace)
        case '{ Assertion.Regex.digit }                                                                => Some(Assertion.Regex.digit)
        case '{ Assertion.Regex.nonDigit }                                                             => Some(Assertion.Regex.nonDigit)
        case '{ Assertion.Regex.literal(${Expr(str)}) }                                                => Some(Assertion.Regex.literal(str))
        case '{ Assertion.Regex.anyCharOf(${Expr(first)}, ${Expr(second)}, ${Varargs(rest)}: _*) }     => Some(Assertion.Regex.anyCharOf(first, second, Varargs(rest).valueOrError*))
        case '{ Assertion.Regex.anyRegexOf(${Expr(first)}, ${Expr(second)}, ${Varargs(rest)}: _*) }    => Some(Assertion.Regex.anyRegexOf(first, second, Varargs(rest).valueOrError*))
        case '{ Assertion.Regex.notAnyCharOf(${Expr(first)}, ${Expr(second)}, ${Varargs(rest)}: _*) }  => Some(Assertion.Regex.notAnyCharOf(first, second, Varargs(rest).valueOrError*))
        case '{ Assertion.Regex.notAnyRegexOf(${Expr(first)}, ${Expr(second)}, ${Varargs(rest)}: _*) } => Some(Assertion.Regex.notAnyRegexOf(first, second, Varargs(rest).valueOrError*))
        case '{ Assertion.Regex.inRange(${Expr(start)}, ${Expr(end)}) }                                => Some(Assertion.Regex.inRange(start, end))
        case '{ Assertion.Regex.notInRange(${Expr(start)}, ${Expr(end)}) }                             => Some(Assertion.Regex.notInRange(start, end))
        case '{ Assertion.Regex.start }                                                                => Some(Assertion.Regex.start)
        case '{ Assertion.Regex.end }                                                                  => Some(Assertion.Regex.end)
        case '{ (${Expr(regex)}: Assertion.Regex).+ }                                                  => Some(regex.+)
        case '{ (${Expr(regex)}: Assertion.Regex).* }                                                  => Some(regex.*)
        case '{ (${Expr(regex)}: Assertion.Regex).? }                                                  => Some(regex.?)
        case '{ (${Expr(regex)}: Assertion.Regex).between(${Expr(min)}, ${Expr(max)}) }                => Some(regex.between(min, max))
        case '{ (${Expr(regex)}: Assertion.Regex).min(${Expr(n)}) }                                    => Some(regex.min(n))
        case '{ (${Expr(regex)}: Assertion.Regex).max(${Expr(n)}) }                                    => Some(regex.max(n))
        case '{ (${Expr(left)}: Assertion.Regex).~(${Expr(right)}: Assertion.Regex) }                  => Some(left ~ right)
        case '{ (${Expr(left)}: Assertion.Regex).|(${Expr(right)}: Assertion.Regex) }                  => Some(left | right)
        case _                                                                                         => None
      }
    }
  }
  
  given [A](using Type[A]): FromExpr[Assertion[A]] with {
    def unapply(assertion: Expr[Assertion[A]])(using Quotes): Option[Assertion[A]] = {
      import quotes.reflect.*

      assertion match {
        case '{ Assertion.anything }                                                     => Some(Assertion.anything)
        case '{ Assertion.between[A](${LiteralUnlift(min)}, ${LiteralUnlift(max)})($_) } => Some(Assertion.between(min, max)(orderingForValue(min)))
        case '{ Assertion.contains(${Expr(value)}) }                                     => Some(Assertion.contains(value).asInstanceOf[Assertion[A]])
        case '{ Assertion.divisibleBy[A](${LiteralUnlift(value)})($_) }                  => Some(Assertion.divisibleBy(value)(numericForValue(value)))
        case '{ Assertion.never }                                                        => Some(Assertion.never)
        case '{ Assertion.endsWith(${Expr(value)}) }                                     => Some(Assertion.endsWith(value).asInstanceOf[Assertion[A]])
        case '{ (${Expr(left)}: Assertion[A]).&&(${Expr(right)}) }                       => Some(Assertion.And(left, right))
        case '{ Assertion.equalTo[A](${LiteralUnlift(value)}) }                          => Some(Assertion.equalTo(value))
        case '{ Assertion.notEqualTo[A](${LiteralUnlift(value)}) }                       => Some(Assertion.notEqualTo(value))
        case '{ Assertion.greaterThan[A](${LiteralUnlift(value)})($_) }                  => Some(Assertion.greaterThan(value)(orderingForValue(value)))
        case '{ Assertion.greaterThanOrEqualTo[A](${LiteralUnlift(value)})($_) }         => Some(Assertion.greaterThanOrEqualTo(value)(orderingForValue(value)))
        case '{ Assertion.hasLength(${Expr(assertion)}) }                                => Some(Assertion.hasLength(assertion).asInstanceOf[Assertion[A]])
        case '{ Assertion.isEmptyString }                                                => Some(Assertion.isEmptyString.asInstanceOf[Assertion[A]])
        case '{ Assertion.lessThan[A](${LiteralUnlift(value)})($_) }                     => Some(Assertion.lessThan(value)(orderingForValue(value)))
        case '{ Assertion.lessThanOrEqualTo[A](${LiteralUnlift(value)})($_) }            => Some(Assertion.lessThanOrEqualTo(value)(orderingForValue(value)))
        case '{ Assertion.matches(${Expr(regex)}: Assertion.Regex) }                     => Some(Assertion.matches(regex).asInstanceOf[Assertion[A]])
        case '{ Assertion.matches((${Expr(regex)}: String).r) }                          => Some(Assertion.matches(regex.r).asInstanceOf[Assertion[A]])
        case '{ Assertion.matches(${Expr(regex)}: String) }                              => Some(Assertion.matches(regex).asInstanceOf[Assertion[A]])
        case '{ !(${Expr(assertion)}: Assertion[A]) }                                    => Some(Assertion.Not(assertion))
        case '{ (${Expr(left)}: Assertion[A]).||(${Expr(right)}) }                       => Some(Assertion.Or(left, right))
        case '{ Assertion.powerOf[A](${LiteralUnlift(value)})($_) }                      => Some(Assertion.powerOf(value)(numericForValue(value)))
        case '{ Assertion.startsWith(${Expr(value)}) }                                   => Some(Assertion.startsWith(value).asInstanceOf[Assertion[A]])
        case _ => None
      }
    }
  }

  object LiteralLift {
    def unapply(any: Any)(using Quotes): Option[Expr[Any]] = any match {
      case int: Int       => Some(Expr(int))
      case string: String => Some(Expr(string))
      case double: Double => Some(Expr(double))
      case float: Float   => Some(Expr(float))
      case long: Long     => Some(Expr(long))
      case short: Short   => Some(Expr(short))
      case byte: Byte     => Some(Expr(byte))
      case _              => None
    }
  }

  object LiteralUnlift {
    def unapply[A: Type](expr: Expr[A])(using Quotes): Option[A] = expr match {
      case '{ ${Expr(int)}: Int }       => Some(int)
      case '{ Int.MaxValue }            => Some(Int.MaxValue.asInstanceOf[A])
      case '{ Int.MinValue }            => Some(Int.MinValue.asInstanceOf[A])
      case '{ ${Expr(string)}: String } => Some(string)
      case '{ ${Expr(double)}: Double } => Some(double)
      case '{ ${Expr(float)}: Float }   => Some(float)
      case '{ ${Expr(long)}: Long }     => Some(long)
      case '{ ${Expr(short)}: Short }   => Some(short)
      case '{ ${Expr(byte)}: Byte }     => Some(byte)
      case '{ ${Expr(char)}: Char }     => Some(char)
      case _                            => None
    }
  }

  private def numericForValue(any: Any)(using Quotes): Numeric[Any] = {
    import quotes.reflect.*

    any match {
      case _: Int    => summon[Numeric[Int]].asInstanceOf[Numeric[Any]]
      case _: Double => summon[Numeric[Double]].asInstanceOf[Numeric[Any]]
      case _: Float  => summon[Numeric[Float]].asInstanceOf[Numeric[Any]]
      case _: Long   => summon[Numeric[Long]].asInstanceOf[Numeric[Any]]
      case _: Short  => summon[Numeric[Short]].asInstanceOf[Numeric[Any]]
      case _: Byte   => summon[Numeric[Byte]].asInstanceOf[Numeric[Any]]
      case _: Char   => summon[Numeric[Char]].asInstanceOf[Numeric[Any]]
      case other     => report.throwError(s"NO NUMERIC FOR $other")
    }
  }

  private def orderingForValue(any: Any)(using Quotes): SOrdering[Any] = {
    import quotes.reflect.*

    any match {
      case _: Int    => SOrdering.Int.asInstanceOf[SOrdering[Any]]
      case _: String => SOrdering.String.asInstanceOf[SOrdering[Any]]
      case _: Double => SOrdering.Double.TotalOrdering.asInstanceOf[SOrdering[Any]]
      case _: Float  => SOrdering.Float.TotalOrdering.asInstanceOf[SOrdering[Any]]
      case _: Long   => SOrdering.Long.asInstanceOf[SOrdering[Any]]
      case _: Short  => SOrdering.Short.asInstanceOf[SOrdering[Any]]
      case _: Byte   => SOrdering.Byte.asInstanceOf[SOrdering[Any]]
      case _: Char   => SOrdering.Char.asInstanceOf[SOrdering[Any]]
      case other     => report.throwError(s"NO ORDERING FOR $other")
    }
  }
}