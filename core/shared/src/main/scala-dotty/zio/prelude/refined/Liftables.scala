package zio.prelude.refined

import scala.quoted.*

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
        case '{ Refinement.Regex.AnyChar }                                                  => Some(Refinement.Regex.AnyChar)
        case '{ Refinement.Regex.anyChar }                                                  => Some(Refinement.Regex.anyChar)
        case '{ Refinement.Regex.Anything }                                                 => Some(Refinement.Regex.Anything)
        case '{ Refinement.Regex.anything }                                                 => Some(Refinement.Regex.anything)
        case '{ Refinement.Regex.End }                                                      => Some(Refinement.Regex.End)
        case '{ Refinement.Regex.end }                                                      => Some(Refinement.Regex.end)
        case '{ Refinement.Regex.Alphanumeric(${Expr(reversed)}) }                          => Some(Refinement.Regex.Alphanumeric(reversed))
        case '{ Refinement.Regex.alphanumeric }                                             => Some(Refinement.Regex.alphanumeric)
        case '{ Refinement.Regex.nonAlphanumeric }                                          => Some(Refinement.Regex.nonAlphanumeric)
        case '{ Refinement.Regex.Whitespace(${Expr(reversed)}) }                            => Some(Refinement.Regex.Whitespace(reversed))
        case '{ Refinement.Regex.whitespace }                                               => Some(Refinement.Regex.whitespace)
        case '{ Refinement.Regex.nonWhitespace }                                            => Some(Refinement.Regex.nonWhitespace)
        case '{ Refinement.Regex.Digit(${Expr(reversed)}) }                                 => Some(Refinement.Regex.Digit(reversed))
        case '{ Refinement.Regex.digit }                                                    => Some(Refinement.Regex.digit)
        case '{ Refinement.Regex.nonDigit }                                                 => Some(Refinement.Regex.nonDigit)
        case '{ Refinement.Regex.Literal(${Expr(char)}) }                                   => Some(Refinement.Regex.Literal(char))
        case '{ Refinement.Regex.literal(${Expr(str)}) }                                    => Some(Refinement.Regex.literal(str))
        case '{ Refinement.Regex.CharacterSet(${Expr(set)}, ${Expr(reversed)}) }            => Some(Refinement.Regex.CharacterSet(set, reversed))
        case '{ Refinement.Regex.anyOf(${Expr(first)}, ${Expr(second)}, ${Expr(rest)}) }    => Some(Refinement.Regex.anyOf(first, second, rest))
        case '{ Refinement.Regex.notAnyOf(${Expr(first)}, ${Expr(second)}, ${Expr(rest)}) } => Some(Refinement.Regex.notAnyOf(first, second, rest))
        case '{ Refinement.Regex.Range(${Expr(start)}, ${Expr(end)}, ${Expr(reversed)}) }   => Some(Refinement.Regex.Range(start, end, reversed))
        case '{ Refinement.Regex.inRange(${Expr(start)}, ${Expr(end)}) }                    => Some(Refinement.Regex.inRange(start, end))
        case '{ Refinement.Regex.notInRange(${Expr(start)}, ${Expr(end)}) }                 => Some(Refinement.Regex.notInRange(start, end))
        case '{ Refinement.Regex.Start }                                                    => Some(Refinement.Regex.Start)
        case '{ Refinement.Regex.start }                                                    => Some(Refinement.Regex.start)
        case '{ Refinement.Regex.Repeat(${Expr(regex)}, ${Expr(min)}, ${Expr(max)}) }       => Some(Refinement.Regex.Repeat(regex, min, max))
        case '{ (${Expr(regex)}: Refinement.Regex).min(${Expr(n)}) }                        => Some(regex.min(n))
        case '{ (${Expr(regex)}: Refinement.Regex).max(${Expr(n)}) }                        => Some(regex.max(n))
        case '{ Refinement.Regex.AndThen(${Expr(first)}, ${Expr(second)}) }                 => Some(Refinement.Regex.AndThen(first, second))
        case '{ (${Expr(left)}: Refinement.Regex).~(${Expr(right)}: Refinement.Regex) }      => Some(left ~ right)
        case '{ Refinement.Regex.OrElse(${Expr(first)}, ${Expr(second)}) }                  => Some(Refinement.Regex.OrElse(first, second))
        case '{ (${Expr(left)}: Refinement.Regex).|(${Expr(right)}: Refinement.Regex) }      => Some(left | right)
        case _                                                                             => None
      }
    }
  }

  given [A](using Type[A]): ToExpr[Refinement[A]] with {
    def apply(assertion: Refinement[A])(using Quotes): Expr[Refinement[A]] = {
      import quotes.reflect.{Refinement => _, *}

      assertion match {
        case Refinement.Always                            => '{ Refinement.Always }
        case Refinement.And(left, right)                  => '{ Refinement.And(${Expr(left.asInstanceOf[Refinement[A]])}, ${Expr(right.asInstanceOf[Refinement[A]])}) }
        case Refinement.EqualTo(LiteralLift(value))       => '{ Refinement.EqualTo($value) }
        case Refinement.GreaterThan(v@LiteralLift(value)) => '{ Refinement.GreaterThan($value)(${orderingForValueExpr(v)}) }
        case Refinement.LessThan(v@LiteralLift(value))    => '{ Refinement.LessThan($value)(${orderingForValueExpr(v)}) }
        case Refinement.Matches(regex)                    => '{ Refinement.Matches(${Expr(regex)}) }
        case Refinement.Not(assertion)                    => '{ Refinement.Not(${Expr(assertion.asInstanceOf[Refinement[A]])}) }
        case Refinement.Or(left, right)                   => '{ Refinement.Or(${Expr(left.asInstanceOf[Refinement[A]])}, ${Expr(right.asInstanceOf[Refinement[A]])}) }
        case _                                           => report.throwError(s"COULD NOT MATCH ASSERTION: $assertion")
      }
    }
  }
  
  given [A](using Type[A]): FromExpr[Refinement[A]] with {
    def unapply(assertion: Expr[Refinement[A]])(using Quotes): Option[Refinement[A]] = {
      import quotes.reflect.{Refinement => _, *}

      assertion match {
        case '{ Refinement.Always }                                                 => Some(Refinement.Always)
        case '{ Refinement.always }                                                 => Some(Refinement.always)
        case '{ Refinement.never }                                                  => Some(Refinement.never)
        case '{ Refinement.And[A](${Expr(left)}, ${Expr(right)}) }                  => Some(Refinement.And(left, right))
        case '{ (${Expr(left)}: Refinement[A]).&&(${Expr(right)}) }                 => Some(Refinement.And(left, right))
        case '{ Refinement.EqualTo[A](${LiteralUnlift(value)}) }                    => Some(Refinement.EqualTo(value))
        case '{ Refinement.equalTo[A](${LiteralUnlift(value)}) }                    => Some(Refinement.equalTo(value))
        case '{ Refinement.notEqualTo[A](${LiteralUnlift(value)}) }                 => Some(Refinement.notEqualTo(value))
        case '{ Refinement.GreaterThan[A](${LiteralUnlift(value)})($_) }            => Some(Refinement.GreaterThan(value)(orderingForValue(value)))
        case '{ Refinement.greaterThan[A](${LiteralUnlift(value)})($_) }            => Some(Refinement.greaterThan(value)(orderingForValue(value)))
        case '{ Refinement.greaterThanOrEqualTo[A](${LiteralUnlift(value)})($_) }   => Some(Refinement.greaterThanOrEqualTo(value)(orderingForValue(value)))
        case '{ Refinement.LessThan[A](${LiteralUnlift(value)})($_) }               => Some(Refinement.LessThan(value)(orderingForValue(value)))
        case '{ Refinement.lessThan[A](${LiteralUnlift(value)})($_) }               => Some(Refinement.lessThan(value)(orderingForValue(value)))
        case '{ Refinement.lessThanOrEqualTo[A](${LiteralUnlift(value)})($_) }      => Some(Refinement.lessThanOrEqualTo(value)(orderingForValue(value)))
        case '{ Refinement.Matches(${Expr(regex)}) }                                => Some(Refinement.Matches(regex).asInstanceOf[Refinement[A]])
        case '{ Refinement.matches(${Expr(regex)}) }                                => Some(Refinement.matches(regex).asInstanceOf[Refinement[A]])
        case '{ Refinement.Not[A](${Expr(assertion)}) }                             => Some(Refinement.Not(assertion))
        case '{ !(${Expr(assertion)}: Refinement[A]) }                              => Some(Refinement.Not(assertion))
        case '{ Refinement.Or[A](${Expr(left)}, ${Expr(right)}) }                   => Some(Refinement.Or(left, right))
        case '{ (${Expr(left)}: Refinement[A]).||(${Expr(right)}) }                 => Some(Refinement.Or(left, right))
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
    def unapply(expr: Expr[Any])(using Quotes): Option[Any] = expr match {
      case '{ ${Expr(int)}: Int }       => Some(int)
      case '{ ${Expr(string)}: String } => Some(string)
      case '{ ${Expr(double)}: Double } => Some(double)
      case '{ ${Expr(float)}: Float }   => Some(float)
      case '{ ${Expr(long)}: Long }     => Some(long)
      case '{ ${Expr(short)}: Short }   => Some(short)
      case '{ ${Expr(byte)}: Byte }     => Some(byte)
      case _                            => None
    }
  }

  private def orderingForValue(any: Any)(using Quotes): Ordering[Any] = {
    import quotes.reflect.*

    any match {
      case _: Int    => Ordering.Int.asInstanceOf[Ordering[Any]]
      case _: String => Ordering.String.asInstanceOf[Ordering[Any]]
      case _: Double => Ordering.Double.TotalOrdering.asInstanceOf[Ordering[Any]]
      case _: Float  => Ordering.Float.TotalOrdering.asInstanceOf[Ordering[Any]]
      case _: Long   => Ordering.Long.asInstanceOf[Ordering[Any]]
      case _: Short  => Ordering.Short.asInstanceOf[Ordering[Any]]
      case _: Byte   => Ordering.Byte.asInstanceOf[Ordering[Any]]
      case other     => report.throwError(s"NO ORDERING FOR $other")
    }
  }

  private def orderingForValueExpr(any: Any)(using Quotes): Expr[Ordering[Any]] = {
    import quotes.reflect.*

    any match {
      case _: Int    => '{ Ordering.Int.asInstanceOf[Ordering[Any]] }
      case _: String => '{ Ordering.String.asInstanceOf[Ordering[Any]] }
      case _: Double => '{ Ordering.Double.TotalOrdering.asInstanceOf[Ordering[Any]] }
      case _: Float  => '{ Ordering.Float.TotalOrdering.asInstanceOf[Ordering[Any]] }
      case _: Long   => '{ Ordering.Long.asInstanceOf[Ordering[Any]] }
      case _: Short  => '{ Ordering.Short.asInstanceOf[Ordering[Any]] }
      case _: Byte   => '{ Ordering.Byte.asInstanceOf[Ordering[Any]] }
      case other     => report.throwError(s"NO ORDERING FOR $other")
    }
  }
}