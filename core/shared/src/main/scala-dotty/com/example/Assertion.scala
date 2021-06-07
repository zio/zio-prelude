package com.example

import scala.quoted.*

enum AssertionError {
  case Failure(condition: String, value: String)
  case And(left: AssertionError, right: AssertionError)
  case Or(left: AssertionError, right: AssertionError)
  case Not(error: AssertionError)

  def &&(that: AssertionError): AssertionError = AssertionError.And(this, that)

  def ||(that: AssertionError): AssertionError = AssertionError.Or(this, that)

  def unary_! : AssertionError = AssertionError.Not(this)

  // TODO see how to use indent level
  def prettyPrint(spaces: Int): String = {
    def loop(indentLevel: Int, error: AssertionError): String =
      error match {
        case and @ And(_, _)           => flattenAnds(and).map(loop(indentLevel, _)).mkString(" && ")
        case or @ Or(_, _)             => flattenOrs(or).map(loop(indentLevel, _)).mkString(" || ")
        case Not(error)                => "!" + loop(indentLevel, error)
        case Failure(condition, value) => s"($condition)"
      }

    loop(0, this)
  }

  private def flattenAnds(error: AssertionError): ::[AssertionError] =
    error match {
      case And(left, right) => (flattenAnds(left) ++ flattenAnds(right)).asInstanceOf[::[AssertionError]]
      case _                => ::(error, Nil)
    }

  private def flattenOrs(error: AssertionError): ::[AssertionError]  =
    error match {
      case Or(left, right) => (flattenOrs(left) ++ flattenOrs(right)).asInstanceOf[::[AssertionError]]
      case _               => ::(error, Nil)
    }
}

object AssertionError {
  def apply(condition: String, value: String) = Failure(condition, value)
}

object Assertion {
  import Literal.*
  given [A](using Type[A]): FromExpr[Assertion[A]] with {
    def unapply(assertion: Expr[Assertion[A]])(using Quotes): Option[Assertion[A]] = {
      import quotes.reflect.{ Literal as _, * }
      assertion match {
        case '{ LessThanEqual[Int]($that, LInt) }                           =>
          Some(LessThanEqual[Int](that.valueOrError, LInt).asInstanceOf[Assertion[A]])
        case '{ LessThanEqual[String]($that, LString) }                     =>
          Some(LessThanEqual[String](that.valueOrError, LString).asInstanceOf[Assertion[A]])
        case '{ LessThanEqual[Double]($that, LDouble) }                     =>
          Some(LessThanEqual[Double](that.valueOrError, LDouble).asInstanceOf[Assertion[A]])
        case '{ LessThanEqual[Float]($that, LFloat) }                       =>
          Some(LessThanEqual[Float](that.valueOrError, LFloat).asInstanceOf[Assertion[A]])
        case '{ LessThanEqual[Long]($that, LLong) }                         =>
          Some(LessThanEqual[Long](that.valueOrError, LLong).asInstanceOf[Assertion[A]])
        case '{ LessThanEqual[Short]($that, LShort) }                       =>
          Some(LessThanEqual[Short](that.valueOrError, LShort).asInstanceOf[Assertion[A]])
        case '{ LessThanEqual[Byte]($that, LByte) }                         =>
          Some(LessThanEqual[Byte](that.valueOrError, LByte).asInstanceOf[Assertion[A]])
        case '{ LessThanEqual[Char]($that, LChar) }                         =>
          Some(LessThanEqual[Char](that.valueOrError, LChar).asInstanceOf[Assertion[A]])
        case '{ lessThanEqual[Int]($that)(using $lit: Literal[Int]) }       =>
          Some(LessThanEqual[Int](that.valueOrError, LInt).asInstanceOf[Assertion[A]])
        case '{ lessThanEqual[String]($that)(using $lit: Literal[String]) } =>
          Some(LessThanEqual[String](that.valueOrError, LString).asInstanceOf[Assertion[A]])
        case '{ lessThanEqual[Double]($that)(using $lit: Literal[Double]) } =>
          Some(LessThanEqual[Double](that.valueOrError, LDouble).asInstanceOf[Assertion[A]])
        case '{ lessThanEqual[Float]($that)(using $lit: Literal[Float]) }   =>
          Some(LessThanEqual[Float](that.valueOrError, LFloat).asInstanceOf[Assertion[A]])
        case '{ lessThanEqual[Long]($that)(using $lit: Literal[Long]) }     =>
          Some(LessThanEqual[Long](that.valueOrError, LLong).asInstanceOf[Assertion[A]])
        case '{ lessThanEqual[Short]($that)(using $lit: Literal[Short]) }   =>
          Some(LessThanEqual[Short](that.valueOrError, LShort).asInstanceOf[Assertion[A]])
        case '{ lessThanEqual[Byte]($that)(using $lit: Literal[Byte]) }     =>
          Some(LessThanEqual[Byte](that.valueOrError, LByte).asInstanceOf[Assertion[A]])
        case '{ lessThanEqual[Char]($that)(using $lit: Literal[Char]) }     =>
          Some(LessThanEqual[Char](that.valueOrError, LChar).asInstanceOf[Assertion[A]])

        case '{ GreaterThanEqual[Int]($that, LInt) }                           =>
          Some(GreaterThanEqual[Int](that.valueOrError, LInt).asInstanceOf[Assertion[A]])
        case '{ GreaterThanEqual[String]($that, LString) }                     =>
          Some(GreaterThanEqual[String](that.valueOrError, LString).asInstanceOf[Assertion[A]])
        case '{ GreaterThanEqual[Double]($that, LDouble) }                     =>
          Some(GreaterThanEqual[Double](that.valueOrError, LDouble).asInstanceOf[Assertion[A]])
        case '{ GreaterThanEqual[Float]($that, LFloat) }                       =>
          Some(GreaterThanEqual[Float](that.valueOrError, LFloat).asInstanceOf[Assertion[A]])
        case '{ GreaterThanEqual[Long]($that, LLong) }                         =>
          Some(GreaterThanEqual[Long](that.valueOrError, LLong).asInstanceOf[Assertion[A]])
        case '{ GreaterThanEqual[Short]($that, LShort) }                       =>
          Some(GreaterThanEqual[Short](that.valueOrError, LShort).asInstanceOf[Assertion[A]])
        case '{ GreaterThanEqual[Byte]($that, LByte) }                         =>
          Some(GreaterThanEqual[Byte](that.valueOrError, LByte).asInstanceOf[Assertion[A]])
        case '{ GreaterThanEqual[Char]($that, LChar) }                         =>
          Some(GreaterThanEqual[Char](that.valueOrError, LChar).asInstanceOf[Assertion[A]])
        case '{ greaterThanEqual[Int]($that)(using $lit: Literal[Int]) }       =>
          Some(GreaterThanEqual[Int](that.valueOrError, LInt).asInstanceOf[Assertion[A]])
        case '{ greaterThanEqual[String]($that)(using $lit: Literal[String]) } =>
          Some(GreaterThanEqual[String](that.valueOrError, LString).asInstanceOf[Assertion[A]])
        case '{ greaterThanEqual[Double]($that)(using $lit: Literal[Double]) } =>
          Some(GreaterThanEqual[Double](that.valueOrError, LDouble).asInstanceOf[Assertion[A]])
        case '{ greaterThanEqual[Float]($that)(using $lit: Literal[Float]) }   =>
          Some(GreaterThanEqual[Float](that.valueOrError, LFloat).asInstanceOf[Assertion[A]])
        case '{ greaterThanEqual[Long]($that)(using $lit: Literal[Long]) }     =>
          Some(GreaterThanEqual[Long](that.valueOrError, LLong).asInstanceOf[Assertion[A]])
        case '{ greaterThanEqual[Short]($that)(using $lit: Literal[Short]) }   =>
          Some(GreaterThanEqual[Short](that.valueOrError, LShort).asInstanceOf[Assertion[A]])
        case '{ greaterThanEqual[Byte]($that)(using $lit: Literal[Byte]) }     =>
          Some(GreaterThanEqual[Byte](that.valueOrError, LByte).asInstanceOf[Assertion[A]])
        case '{ greaterThanEqual[Char]($that)(using $lit: Literal[Char]) }     =>
          Some(GreaterThanEqual[Char](that.valueOrError, LChar).asInstanceOf[Assertion[A]])

        case '{ And[A]($left, $right) }            => Some(And(left.valueOrError, right.valueOrError))
        case '{ ($left: Assertion[A]).&&($right) } => Some(And(left.valueOrError, right.valueOrError))

        case '{ Or[A]($left, $right) }             => Some(Or(left.valueOrError, right.valueOrError))
        case '{ ($left: Assertion[A]).||($right) } => Some(Or(left.valueOrError, right.valueOrError))

        case '{ Not[A]($assertion) }          => Some(Not(assertion.valueOrError))
        case '{ !($assertion: Assertion[A]) } => Some(Not(assertion.valueOrError))

        case '{ True }   => Some(True)
        case '{ isTrue } => Some(True)

        case '{ Matches($regex) } => Some(Matches(regex.valueOrError).asInstanceOf[Assertion[A]])
        case '{ matches($regex) } => Some(Matches(regex.valueOrError).asInstanceOf[Assertion[A]])

        case '{ equal[Char]($that)(using $lit: Literal[Char]) } =>
          Some(equal(that.valueOrError)(using LChar).asInstanceOf[Assertion[A]])
        case '{ isAlpha }                                       => Some(isAlpha.asInstanceOf[Assertion[A]])
        case '{ isDigit }                                       => Some(isDigit.asInstanceOf[Assertion[A]])
        // TODO add other constructors from DSL

        case _ => None
      }
    }
  }

  given [A](using Type[A]): ToExpr[Assertion[A]] with {
    def apply(assertion: Assertion[A])(using Quotes) =
      assertion match {
        case LessThanEqual(that, LInt)    => '{ LessThanEqual(${ Expr(that.asInstanceOf[Int]) }.asInstanceOf[A], LInt) }
        case LessThanEqual(that, LString) =>
          '{ LessThanEqual(${ Expr(that.asInstanceOf[String]) }.asInstanceOf[A], LString) }
        case LessThanEqual(that, LDouble) =>
          '{ LessThanEqual(${ Expr(that.asInstanceOf[Double]) }.asInstanceOf[A], LDouble) }
        case LessThanEqual(that, LFloat)  =>
          '{ LessThanEqual(${ Expr(that.asInstanceOf[Float]) }.asInstanceOf[A], LFloat) }
        case LessThanEqual(that, LLong)   => '{ LessThanEqual(${ Expr(that.asInstanceOf[Long]) }.asInstanceOf[A], LLong) }
        case LessThanEqual(that, LShort)  =>
          '{ LessThanEqual(${ Expr(that.asInstanceOf[Short]) }.asInstanceOf[A], LShort) }
        case LessThanEqual(that, LByte)   => '{ LessThanEqual(${ Expr(that.asInstanceOf[Byte]) }.asInstanceOf[A], LByte) }
        case LessThanEqual(that, LChar)   => '{ LessThanEqual(${ Expr(that.asInstanceOf[Char]) }.asInstanceOf[A], LChar) }

        case GreaterThanEqual(that, LInt)    =>
          '{ GreaterThanEqual(${ Expr(that.asInstanceOf[Int]) }.asInstanceOf[A], LInt) }
        case GreaterThanEqual(that, LString) =>
          '{ GreaterThanEqual(${ Expr(that.asInstanceOf[String]) }.asInstanceOf[A], LString) }
        case GreaterThanEqual(that, LDouble) =>
          '{ GreaterThanEqual(${ Expr(that.asInstanceOf[Double]) }.asInstanceOf[A], LDouble) }
        case GreaterThanEqual(that, LFloat)  =>
          '{ GreaterThanEqual(${ Expr(that.asInstanceOf[Float]) }.asInstanceOf[A], LFloat) }
        case GreaterThanEqual(that, LLong)   =>
          '{ GreaterThanEqual(${ Expr(that.asInstanceOf[Long]) }.asInstanceOf[A], LLong) }
        case GreaterThanEqual(that, LShort)  =>
          '{ GreaterThanEqual(${ Expr(that.asInstanceOf[Short]) }.asInstanceOf[A], LShort) }
        case GreaterThanEqual(that, LByte)   =>
          '{ GreaterThanEqual(${ Expr(that.asInstanceOf[Byte]) }.asInstanceOf[A], LByte) }
        case GreaterThanEqual(that, LChar)   =>
          '{ GreaterThanEqual(${ Expr(that.asInstanceOf[Char]) }.asInstanceOf[A], LChar) }

        case And(left, right) =>
          '{ And(${ Expr(left.asInstanceOf[Assertion[A]]) }, ${ Expr(right.asInstanceOf[Assertion[A]]) }) }
        case Or(left, right)  =>
          '{ Or(${ Expr(left.asInstanceOf[Assertion[A]]) }, ${ Expr(right.asInstanceOf[Assertion[A]]) }) }
        case Not(assertion)   => '{ Not(${ Expr(assertion.asInstanceOf[Assertion[A]]) }) }
        case True             => '{ True }
        case Matches(regex)   => '{ Matches(${ Expr(regex) }).asInstanceOf[Assertion[A]] }
      }
  }

  def lessThan[A](value: A)(using lit: Literal[A]): Assertion[A]         = lessThanEqual(value) && notEqual(value)
  def lessThanEqual[A](value: A)(using lit: Literal[A]): Assertion[A]    = Assertion.LessThanEqual(value, lit)
  def greaterThan[A](value: A)(using lit: Literal[A]): Assertion[A]      = greaterThanEqual(value) && notEqual(value)
  def greaterThanEqual[A](value: A)(using lit: Literal[A]): Assertion[A] = Assertion.GreaterThanEqual(value, lit)
  def equal[A](value: A)(using lit: Literal[A]): Assertion[A]            = lessThanEqual(value) && greaterThanEqual(value)
  def notEqual[A](value: A)(using lit: Literal[A]): Assertion[A]         = !equal(value)
  def matches(regex: Regex): Assertion[String]                           = Assertion.Matches(regex)
  def isTrue: Assertion[Any]                                             = Assertion.True
  def isFalse: Assertion[Any]                                            = !isTrue
  def anyOf[A](values: A*)(using lit: Literal[A]): Assertion[A]          =
    values.foldLeft[Assertion[A]](isTrue)((acc, a) => acc || equal(a))
  val isAlpha: Assertion[Char]                                           = anyOf(('A' to 'z')*)
  val isDigit: Assertion[Char]                                           = anyOf(('0' to '9')*)

  enum Literal[+A] {
    case LInt extends Literal[Int]
    case LString extends Literal[String]
    case LDouble extends Literal[Double]
    case LFloat extends Literal[Float]
    case LLong extends Literal[Long]
    case LShort extends Literal[Short]
    case LByte extends Literal[Byte]
    case LChar extends Literal[Char]
  }
  object Literal {
    inline given Literal[Int]    = LInt
    inline given Literal[String] = LString
    inline given Literal[Double] = LDouble
    inline given Literal[Float]  = LFloat
    inline given Literal[Long]   = LLong
    inline given Literal[Short]  = LShort
    inline given Literal[Byte]   = LByte
    inline given Literal[Char]   = LChar
  }

  enum CharacterClass {
    case Literal(first: Char, rest: List[Char])
    case Negation(charClass: CharacterClass)
    case Or(left: CharacterClass, right: CharacterClass)
    case And(left: CharacterClass, right: CharacterClass)
    case Range(min: Char, max: Char)
  }

  enum Regex {
    case Character(assertion: Assertion[Char])
    case Repeat(regex: Regex, min: Option[Int], max: Option[Int])
    case Sequence(first: Regex, second: Regex)

    // [a-zA-Z]
    // Or(And(GreaterThanEqual(a), LessThanEqual(a)), And(GreaterThanEqual(A), LessThanEqual(Z)))

    /*
        Character classes
        [abc]	        a, b, or c (simple class)
        [^abc]	        Any character except a, b, or c (negation)
        [a-zA-Z]	    a through z or A through Z, inclusive (range)
        [a-d[m-p]]	    a through d, or m through p: [a-dm-p] (union)
        [a-z&&[def]]	d, e, or f (intersection)
        [a-z&&[^bc]]	a through z, except for b and c: [ad-z] (subtraction)
        [a-z&&[^m-p]]	a through z, and not m through p: [a-lq-z](subtraction)
     */

    /*
        Characters
        x	The character x
        \\	The backslash character
        \0n	The character with octal value 0n (0 <= n <= 7)
        \0nn	The character with octal value 0nn (0 <= n <= 7)
        \0mnn	The character with octal value 0mnn (0 <= m <= 3, 0 <= n <= 7)
        \xhh	The character with hexadecimal value 0xhh
        \uhhhh	The character with hexadecimal value 0xhhhh
        \x{h...h}	The character with hexadecimal value 0xh...h (Character.MIN_CODE_POINT  <= 0xh...h <=  Character.MAX_CODE_POINT)
        \t	The tab character ('\u0009')
        \n	The newline (line feed) character ('\u000A')
        \r	The carriage-return character ('\u000D')
        \f	The form-feed character ('\u000C')
        \a	The alert (bell) character ('\u0007')
        \e	The escape character ('\u001B')
        \cx	The control character corresponding to x
     */
    def prettyPrint: String = {
      def render(assertion: Assertion[Char]): CharacterClass =
        assertion match {
          case And(left, right)          => ???
          case Or(left, right)           => ???
          case Not(assertion)            => ???
          case LessThanEqual(that, _)    => ???
          case GreaterThanEqual(that, _) => ???
          case True                      => ???
          case Matches(_)                => ???
        }

      this match {
        case Character(assertion)    => ""
        case Repeat(regex, min, max) =>
          regex.prettyPrint + ((min, max) match {
            case (Some(min), Some(max)) =>
              s"{$min,$max}"
            case (Some(1), None)        =>
              "+"
            case (Some(min), None)      =>
              s"{$min,}"
            case (None, Some(max))      =>
              s"{0,$max}"
            case (None, None)           => "*"
          }
          )
        case Sequence(first, second) =>
          first.prettyPrint + second.prettyPrint
      }
    }

    def ~(that: Regex): Regex = Sequence(this, that)

    def min(n: Int)                                =
      this match {
        case Repeat(regex, _, max) => Repeat(regex, Some(n), max)
        case regex                 => Repeat(regex, Some(n), None)
      }

    def max(n: Int)                                =
      this match {
        case Repeat(regex, min, _) => Repeat(regex, min, Some(n))
        case regex                 => Repeat(regex, None, Some(n))
      }

    val compiled: List[Char] => Option[List[Char]] =
      this match {
        case Assertion.Regex.Character(assertion)    => {
          case head :: tail if assertion(head).isRight => Some(tail)
          case _                                       => None
        }
        case Assertion.Regex.Repeat(regex, min, max) =>
          def loop(input: List[Char], count: Int): Option[List[Char]] =
            (regex.compiled(input), max) match {
              case (Some(input), Some(max)) if count >= max => Some(input)
              case (Some(input), _)                         => loop(input, count + 1)
              case (None, Some(min)) if count >= min        => Some(input)
              case (None, Some(min))                        => loop(input, count + 1)
              case (None, None)                             => Some(input)
            }
          loop(_, 0)

        case Assertion.Regex.Sequence(first, second) =>
          first.compiled(_).flatMap(second.compiled)
      }
  }
  object Regex {
    given FromExpr[Regex] with {
      def unapply(assertion: Expr[Regex])(using Quotes): Option[Regex] = {
        import quotes.reflect.*
        assertion match {
          case '{ Character($assertion) } => Some(Character(assertion.valueOrError))
          case '{ char($assertion) }      => Some(Character(assertion.valueOrError))

          case '{ Repeat($regex, Some(${ Expr(min) }), Some(${ Expr(max) })) } =>
            Some(Repeat(regex.valueOrError, Some(min), Some(max)))
          case '{ Repeat($regex, Some(${ Expr(min) }), None) }                 => Some(Repeat(regex.valueOrError, Some(min), None))
          case '{ Repeat($regex, None, Some(${ Expr(max) })) }                 => Some(Repeat(regex.valueOrError, None, Some(max)))
          case '{ Repeat($regex, None, None) }                                 => Some(Repeat(regex.valueOrError, None, None))

          case '{ ($regex: Regex).min($n) } => Some(regex.valueOrError.min(n.valueOrError))
          case '{ ($regex: Regex).max($n) } => Some(regex.valueOrError.max(n.valueOrError))

          case '{ Sequence($first, $second) }  => Some(Sequence(first.valueOrError, second.valueOrError))
          case '{ ($first: Regex).~($second) } => Some(Sequence(first.valueOrError, second.valueOrError))

          case _ => None
        }
      }
    }

    given ToExpr[Regex] with {
      def apply(regex: Regex)(using Quotes) =
        regex match {
          case Character(assertion)    => '{ Character(${ Expr(assertion) }) }
          case Repeat(regex, min, max) => '{ Repeat(${ Expr(regex) }, ${ Expr(min) }, ${ Expr(max) }) }
          case Sequence(first, second) => '{ Sequence(${ Expr(first) }, ${ Expr(second) }) }
        }
    }

    def char(assertion: Assertion[Char]): Regex = Regex.Character(assertion)  }
}
