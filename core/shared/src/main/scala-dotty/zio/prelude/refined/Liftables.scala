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
      import quotes.reflect.*

      assertion match {
        case '{ Assertion.Regex.AnyChar }                                                  => Some(Assertion.Regex.AnyChar)
        case '{ Assertion.Regex.anyChar }                                                  => Some(Assertion.Regex.anyChar)
        case '{ Assertion.Regex.Anything }                                                 => Some(Assertion.Regex.Anything)
        case '{ Assertion.Regex.anything }                                                 => Some(Assertion.Regex.anything)
        case '{ Assertion.Regex.End }                                                      => Some(Assertion.Regex.End)
        case '{ Assertion.Regex.end }                                                      => Some(Assertion.Regex.end)
        case '{ Assertion.Regex.Alphanumeric(${Expr(reversed)}) }                          => Some(Assertion.Regex.Alphanumeric(reversed))
        case '{ Assertion.Regex.alphanumeric }                                             => Some(Assertion.Regex.alphanumeric)
        case '{ Assertion.Regex.nonAlphanumeric }                                          => Some(Assertion.Regex.nonAlphanumeric)
        case '{ Assertion.Regex.Whitespace(${Expr(reversed)}) }                            => Some(Assertion.Regex.Whitespace(reversed))
        case '{ Assertion.Regex.whitespace }                                               => Some(Assertion.Regex.whitespace)
        case '{ Assertion.Regex.nonWhitespace }                                            => Some(Assertion.Regex.nonWhitespace)
        case '{ Assertion.Regex.Digit(${Expr(reversed)}) }                                 => Some(Assertion.Regex.Digit(reversed))
        case '{ Assertion.Regex.digit }                                                    => Some(Assertion.Regex.digit)
        case '{ Assertion.Regex.nonDigit }                                                 => Some(Assertion.Regex.nonDigit)
        case '{ Assertion.Regex.Literal(${Expr(char)}) }                                   => Some(Assertion.Regex.Literal(char))
        case '{ Assertion.Regex.literal(${Expr(str)}) }                                    => Some(Assertion.Regex.literal(str))
        case '{ Assertion.Regex.CharacterSet(${Expr(set)}, ${Expr(reversed)}) }            => Some(Assertion.Regex.CharacterSet(set, reversed))
        case '{ Assertion.Regex.anyOf(${Expr(first)}, ${Expr(second)}, ${Expr(rest)}) }    => Some(Assertion.Regex.anyOf(first, second, rest))
        case '{ Assertion.Regex.notAnyOf(${Expr(first)}, ${Expr(second)}, ${Expr(rest)}) } => Some(Assertion.Regex.notAnyOf(first, second, rest))
        case '{ Assertion.Regex.Range(${Expr(start)}, ${Expr(end)}, ${Expr(reversed)}) }   => Some(Assertion.Regex.Range(start, end, reversed))
        case '{ Assertion.Regex.inRange(${Expr(start)}, ${Expr(end)}) }                    => Some(Assertion.Regex.inRange(start, end))
        case '{ Assertion.Regex.notInRange(${Expr(start)}, ${Expr(end)}) }                 => Some(Assertion.Regex.notInRange(start, end))
        case '{ Assertion.Regex.Start }                                                    => Some(Assertion.Regex.Start)
        case '{ Assertion.Regex.start }                                                    => Some(Assertion.Regex.start)
        case '{ Assertion.Regex.Repeat(${Expr(regex)}, ${Expr(min)}, ${Expr(max)}) }       => Some(Assertion.Regex.Repeat(regex, min, max))
        case '{ (${Expr(regex)}: Assertion.Regex).min(${Expr(n)}) }                        => Some(regex.min(n))
        case '{ (${Expr(regex)}: Assertion.Regex).max(${Expr(n)}) }                        => Some(regex.max(n))
        case '{ Assertion.Regex.AndThen(${Expr(first)}, ${Expr(second)}) }                 => Some(Assertion.Regex.AndThen(first, second))
        case '{ (${Expr(left)}: Assertion.Regex).~(${Expr(right)}: Assertion.Regex) }      => Some(left ~ right)
        case '{ Assertion.Regex.OrElse(${Expr(first)}, ${Expr(second)}) }                  => Some(Assertion.Regex.OrElse(first, second))
        case '{ (${Expr(left)}: Assertion.Regex).|(${Expr(right)}: Assertion.Regex) }      => Some(left | right)
        case _                                                                             => None
      }
    }
  }

  given [A](using Type[A]): ToExpr[Refinement[A]] with {
    def apply(assertion: Refinement[A])(using Quotes): Expr[Refinement[A]] = {
      import quotes.reflect.*

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
      import quotes.reflect.*

      assertion match {
        case '{ Assertion.Always }                                                 => Some(Assertion.Always)
        case '{ Assertion.always }                                                 => Some(Assertion.always)
        case '{ Assertion.never }                                                  => Some(Assertion.never)
        case '{ Assertion.And[A](${Expr(left)}, ${Expr(right)}) }                  => Some(Assertion.And(left, right))
        case '{ (${Expr(left)}: Assertion[A]).&&(${Expr(right)}) }                 => Some(Assertion.And(left, right))
        case '{ Assertion.EqualTo[A](${LiteralUnlift(value)}) }                    => Some(Assertion.EqualTo(value))
        case '{ Assertion.equalTo[A](${LiteralUnlift(value)}) }                    => Some(Assertion.equalTo(value))
        case '{ Assertion.notEqualTo[A](${LiteralUnlift(value)}) }                 => Some(Assertion.notEqualTo(value))
        case '{ Assertion.GreaterThan[A](${LiteralUnlift(value)})($_) }            => Some(Assertion.GreaterThan(value)(orderingForValue(value)))
        case '{ Assertion.greaterThan[A](${LiteralUnlift(value)})($_) }            => Some(Assertion.greaterThan(value)(orderingForValue(value)))
        case '{ Assertion.greaterThanOrEqualTo[A](${LiteralUnlift(value)})($_) }   => Some(Assertion.greaterThanOrEqualTo(value)(orderingForValue(value)))
        case '{ Assertion.LessThan[A](${LiteralUnlift(value)})($_) }               => Some(Assertion.LessThan(value)(orderingForValue(value)))
        case '{ Assertion.lessThan[A](${LiteralUnlift(value)})($_) }               => Some(Assertion.lessThan(value)(orderingForValue(value)))
        case '{ Assertion.lessThanOrEqualTo[A](${LiteralUnlift(value)})($_) }      => Some(Assertion.lessThanOrEqualTo(value)(orderingForValue(value)))
        case '{ Assertion.Matches(${Expr(regex)}) }                                => Some(Assertion.Matches(regex).asInstanceOf[Assertion[A]])
        case '{ Assertion.matches(${Expr(regex)}) }                                => Some(Assertion.matches(regex).asInstanceOf[Assertion[A]])
        case '{ Assertion.Not[A](${Expr(assertion)}) }                             => Some(Assertion.Not(assertion))
        case '{ !(${Expr(assertion)}: Assertion[A]) }                              => Some(Assertion.Not(assertion))
        case '{ Assertion.Or[A](${Expr(left)}, ${Expr(right)}) }                   => Some(Assertion.Or(left, right))
        case '{ (${Expr(left)}: Assertion[A]).||(${Expr(right)}) }                 => Some(Assertion.Or(left, right))
        case _ => None
      }
    }
  }

  given [A, T](using Type[A], Type[T]): FromExpr[QuotedRefinement[A, T]] with {
    def unapply(quotedAssertion: Expr[QuotedRefinement[A, T]])(using Quotes): Option[QuotedRefinement[A, T]] =
      quotedAssertion match {
        case '{ QuotedAssertion[A, T](${Expr(assertion)}) } => Some(QuotedAssertion(assertion))
        case '{ Refined[A, T](${Expr(assertion)}) }         => Some(QuotedAssertion(assertion))
        case _                                              => None
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