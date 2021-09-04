package zio.prelude

import scala.quoted.*
import scala.math.{Ordering => SOrdering}

trait Liftables {
  given ToExpr[Refinement.Regex] with {
    def apply(regex: Refinement.Regex)(using Quotes): Expr[Refinement.Regex] =
      regex match {
        case Refinement.Regex.AnyChar                     => '{ Refinement.Regex.AnyChar }
        case Refinement.Regex.Anything                    => '{ Refinement.Regex.Anything }
        case Refinement.Regex.End                         => '{ Refinement.Regex.End }
        case Refinement.Regex.Alphanumeric(reversed)      => '{ Refinement.Regex.Alphanumeric(${Expr(reversed)}) }
        case Refinement.Regex.Whitespace(reversed)        => '{ Refinement.Regex.Whitespace(${Expr(reversed)}) }
        case Refinement.Regex.Digit(reversed)             => '{ Refinement.Regex.Digit(${Expr(reversed)}) }
        case Refinement.Regex.Literal(char)               => '{ Refinement.Regex.Literal(${Expr(char)}) }
        case Refinement.Regex.CharacterSet(set, reversed) => '{ Refinement.Regex.CharacterSet(${Expr(set)}, ${Expr(reversed)}) }
        case Refinement.Regex.Range(start, end, reversed) => '{ Refinement.Regex.Range(${Expr(start)}, ${Expr(end)}, ${Expr(reversed)}) }
        case Refinement.Regex.Start                       => '{ Refinement.Regex.Start }
        case Refinement.Regex.Repeat(regex, min, max)     => '{ Refinement.Regex.Repeat(${Expr(regex)}, ${Expr(min)}, ${Expr(max)}) }
        case Refinement.Regex.AndThen(first, second)      => '{ Refinement.Regex.AndThen(${Expr(first)}, ${Expr(second)}) }
        case Refinement.Regex.OrElse(first, second)       => '{ Refinement.Regex.OrElse(${Expr(first)}, ${Expr(second)}) }
      }
  }

  given FromExpr[Refinement.Regex] with {
    def unapply(assertion: Expr[Refinement.Regex])(using Quotes): Option[Refinement.Regex] = {
      import quotes.reflect.{Refinement => _, *}

      assertion match {
        case '{ Refinement.Regex.anyChar }                                                  => Some(Refinement.Regex.anyChar)
        case '{ Refinement.Regex.anything }                                                 => Some(Refinement.Regex.anything)
        case '{ Refinement.Regex.alphanumeric }                                             => Some(Refinement.Regex.alphanumeric)
        case '{ Refinement.Regex.nonAlphanumeric }                                          => Some(Refinement.Regex.nonAlphanumeric)
        case '{ Refinement.Regex.whitespace }                                               => Some(Refinement.Regex.whitespace)
        case '{ Refinement.Regex.nonWhitespace }                                            => Some(Refinement.Regex.nonWhitespace)
        case '{ Refinement.Regex.digit }                                                    => Some(Refinement.Regex.digit)
        case '{ Refinement.Regex.nonDigit }                                                 => Some(Refinement.Regex.nonDigit)
        case '{ Refinement.Regex.literal(${Expr(str)}) }                                    => Some(Refinement.Regex.literal(str))
        case '{ Refinement.Regex.anyOf(${Expr(first)}, ${Expr(second)}, ${Expr(rest)}) }    => Some(Refinement.Regex.anyOf(first, second, rest))
        case '{ Refinement.Regex.notAnyOf(${Expr(first)}, ${Expr(second)}, ${Expr(rest)}) } => Some(Refinement.Regex.notAnyOf(first, second, rest))
        case '{ Refinement.Regex.inRange(${Expr(start)}, ${Expr(end)}) }                    => Some(Refinement.Regex.inRange(start, end))
        case '{ Refinement.Regex.notInRange(${Expr(start)}, ${Expr(end)}) }                 => Some(Refinement.Regex.notInRange(start, end))
        case '{ (${Expr(regex)}: Refinement.Regex).+ }                                      => Some(regex.+)
        case '{ (${Expr(regex)}: Refinement.Regex).* }                                      => Some(regex.*)
        case '{ (${Expr(regex)}: Refinement.Regex).between(${Expr(min)}, ${Expr(max)}) }    => Some(regex.between(min, max))
        case '{ (${Expr(regex)}: Refinement.Regex).min(${Expr(n)}) }                        => Some(regex.min(n))
        case '{ (${Expr(regex)}: Refinement.Regex).max(${Expr(n)}) }                        => Some(regex.max(n))
        case '{ (${Expr(left)}: Refinement.Regex).~(${Expr(right)}: Refinement.Regex) }     => Some(left ~ right)
        case '{ (${Expr(left)}: Refinement.Regex).|(${Expr(right)}: Refinement.Regex) }     => Some(left | right)
        case _                                                                              => None
      }
    }
  }
  
  given [A](using Type[A]): FromExpr[Refinement[A]] with {
    def unapply(assertion: Expr[Refinement[A]])(using Quotes): Option[Refinement[A]] = {
      import quotes.reflect.{Refinement => _, *}

      assertion match {
        case '{ Refinement.anything }                                                     => Some(Refinement.anything)
        case '{ Refinement.between[A](${LiteralUnlift(min)}, ${LiteralUnlift(max)})($_) } => Some(Refinement.between(min, max)(orderingForValue(min)))
        case '{ Refinement.never }                                                        => Some(Refinement.never)
        case '{ (${Expr(left)}: Refinement[A]).&&(${Expr(right)}) }                       => Some(Refinement.And(left, right))
        case '{ Refinement.equalTo[A](${LiteralUnlift(value)}) }                          => Some(Refinement.equalTo(value))
        case '{ Refinement.notEqualTo[A](${LiteralUnlift(value)}) }                       => Some(Refinement.notEqualTo(value))
        case '{ Refinement.greaterThan[A](${LiteralUnlift(value)})($_) }                  => Some(Refinement.greaterThan(value)(orderingForValue(value)))
        case '{ Refinement.greaterThanOrEqualTo[A](${LiteralUnlift(value)})($_) }         => Some(Refinement.greaterThanOrEqualTo(value)(orderingForValue(value)))
        case '{ Refinement.lessThan[A](${LiteralUnlift(value)})($_) }                     => Some(Refinement.lessThan(value)(orderingForValue(value)))
        case '{ Refinement.lessThanOrEqualTo[A](${LiteralUnlift(value)})($_) }            => Some(Refinement.lessThanOrEqualTo(value)(orderingForValue(value)))
        case '{ Refinement.matches(${Expr(regex)}: Refinement.Regex) }                    => Some(Refinement.matches(regex).asInstanceOf[Refinement[A]])
        case '{ Refinement.matches((${Expr(regex)}: String).r) }                          => Some(Refinement.matches(regex.r).asInstanceOf[Refinement[A]])
        case '{ Refinement.matches(${Expr(regex)}: String) }                              => Some(Refinement.matches(regex).asInstanceOf[Refinement[A]])
        case '{ !(${Expr(assertion)}: Refinement[A]) }                                    => Some(Refinement.Not(assertion))
        case '{ (${Expr(left)}: Refinement[A]).||(${Expr(right)}) }                       => Some(Refinement.Or(left, right))
        case '{ Refinement.powerOf[A](${LiteralUnlift(value)})($_) }                      => Some(Refinement.powerOf(value)(numericForValue(value)))
        case _ => None
      }
    }
  }

  // given [A, T](using Type[A], Type[T]): FromExpr[QuotedRefinement[A, T]] with {
  //   def unapply(quotedRefinement: Expr[QuotedRefinement[A, T]])(using Quotes): Option[QuotedRefinement[A, T]] =
  //     quotedRefinement match {
  //       case '{ QuotedRefinement[A, T](${Expr(assertion)}) } => Some(QuotedRefinement(assertion))
  //       case '{ Refined[A, T](${Expr(assertion)}) }         => Some(QuotedRefinement(assertion))
  //       case _                                              => None
  //     }
  // }

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