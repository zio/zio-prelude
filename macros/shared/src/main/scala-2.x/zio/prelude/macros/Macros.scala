package zio.prelude.macros

import zio.prelude.refined._
import zio.prelude.refined.macros.QuotedAssertion

import scala.reflect.macros.whitebox

class Macros(val c: whitebox.Context) {
  import c.universe.{TypeTag => _, _}
  val AssertionM = q"_root_.zio.prelude.refined.Assertion"

  def smartApply[A: WeakTypeTag, Meta: WeakTypeTag](value: c.Tree): c.Tree = {
    val tree = c.weakTypeOf[Meta].decls.flatMap(_.annotations).headOption.flatMap(_.tree.children.lastOption)
    println(tree)
    tree match {
      case Some(q"${assertionC: Assertion[Any]}") =>
        literalUnlifter.lift(value) match {
          case Some(valueC) =>
            assertionC(valueC) match {
              case Left(error) =>
                val header  =
                  scala.Console.BOLD + scala.Console.RED + scala.Console.REVERSED + "  Assertion Failed  " + scala.Console.RESET
                val message = s"""
                                 |$header
                                 |${error.render}
                                 |""".stripMargin

                c.abort(c.enclosingPosition, message)
              case Right(_)    =>
                q"${c.prefix}.unsafeApply($value)"
            }
          case None         =>
            val header  =
              scala.Console.BOLD + scala.Console.RED + scala.Console.REVERSED + "  Assertion Failed  " + scala.Console.RESET
            val message = s"""
                             |$header
                             |Could not validate Smart Assertion at compile-time.
                             |Either use a literal or call ${scala.Console.BLUE + s"${c.prefix.tree}.wrapEither(${value})" + scala.Console.RESET}
                             |""".stripMargin

            c.abort(c.enclosingPosition, message)
        }

      case _ =>
        c.abort(c.enclosingPosition, "MISSING ASSERTION")
    }

  }

  def makeRefined[A: WeakTypeTag](assertion: c.Tree): c.Tree =
    assertion match {
      case q"${assertionC: Assertion[Any]}" =>
        val anonTrait = TypeName(c.freshName("anonTrait"))
        q"""
trait $anonTrait {
  @${c.weakTypeOf[QuotedAssertion]}($assertionC)
  def fun: Int = 12
}

new _root_.zio.prelude.macros.Refined[${c.weakTypeOf[A]}, $anonTrait] {
  @${c.weakTypeOf[QuotedAssertion]}($assertionC)
  def assertion = $assertionC
}
             """

      case _ =>
        c.abort(c.enclosingPosition, s"FAILED TO UNLIFT ASSERTION: $assertion")
    }

  object Literal {
    def unapply(tree: Tree): Option[Any] = literalUnlifter.lift(tree)
  }

  val literalUnlifter: PartialFunction[Tree, Any] = {
    case q"${int: Int}"                                     => int
    case q"${string: String}"                               => string
    case q"${boolean: Boolean}"                             => boolean
    case q"scala.Some.apply[$_](${Literal(value)})" => Some(value)
    case q"scala.None"                                      => None
  }

  def orderingForValue(any: Any): Option[Ordering[Any]] = any match {
    case _: Int    => Some(scala.Ordering.Int.asInstanceOf[Ordering[Any]])
    case _: String => Some(scala.Ordering.String.asInstanceOf[Ordering[Any]])
    case ord       => c.abort(c.enclosingPosition, s"NO ORDERING FOR $ord")
  }

  object LiteralLift {
    def unapply(any: Any): Option[Tree] =
      literalLifter.lift(any)
  }

  val literalLifter: PartialFunction[Any, Tree] = {
    case int: Int       => q"$int"
    case string: String => q"$string"
  }

  val orderingLifter: PartialFunction[Any, Tree] = {
    case _: Ordering.Int.type    => q"Ordering.Int"
    case _: Ordering.String.type => q"Ordering.String"
  }

  implicit lazy val liftAssertion: Liftable[Assertion[Any]] =
    Liftable[Assertion[Any]] {
      case Assertion.EqualTo(LiteralLift(value))     =>
        q"$AssertionM.EqualTo($value)"
      case matches: Assertion.Matches                  =>
        q"$AssertionM.Matches(${matches.regex})"
      case Assertion.GreaterThan(LiteralLift(value)) =>
        q"$AssertionM.GreaterThan($value)"
      case Assertion.LessThan(LiteralLift(value))    =>
        q"$AssertionM.LessThan($value)"
      case Assertion.And(lhs, rhs)                     =>
        q"$AssertionM.And($lhs, $rhs)"
      case Assertion.Or(lhs, rhs)                      =>
        q"$AssertionM.Or($lhs, $rhs)"
      case _                                           =>
        c.abort(c.enclosingPosition, s"FAILED TO LIFT ASSERTION")
    }

  implicit lazy val unliftAssertion: Unliftable[Assertion[Any]] =
    Unliftable[Assertion[Any]] {
      case q"zio.prelude.refined.Assertion.EqualTo.apply[$_](${Literal(value)})" =>
        Assertion.EqualTo(value)

      case q"zio.prelude.refined.Assertion.equalTo[$_](${Literal(value)})" =>
        Assertion.EqualTo(value)

      case q"zio.prelude.refined.Assertion.lessThan[$_](${Literal(value)})($_)" =>
        Assertion.LessThan(value)(orderingForValue(value).get)

      case q"zio.prelude.refined.Assertion.LessThan.apply[$_](${Literal(value)})($_)" =>
        Assertion.LessThan(value)(orderingForValue(value).get)

      case q"zio.prelude.refined.Assertion.greaterThan[$_](${Literal(value)})($_)" =>
        Assertion.GreaterThan(value)(orderingForValue(value).get)

      case q"zio.prelude.refined.Assertion.GreaterThan.apply[$_](${Literal(value)})($_)" =>
        Assertion.GreaterThan(value)(orderingForValue(value).get)

      case q"zio.prelude.refined.Assertion.matches(${regex: String})" =>
        Assertion.Matches(regex).asInstanceOf[Assertion[Any]]

      case q"zio.prelude.refined.Assertion.Matches.apply(${regex: String})" =>
        Assertion.Matches(regex).asInstanceOf[Assertion[Any]]

      case q"zio.prelude.refined.Assertion.And.apply[$_](${unliftAssertion(left)}, ${unliftAssertion(right)})" =>
        Assertion.And(left, right)

      case q"${unliftAssertion(left)}.&&[$_](${unliftAssertion(right)})" =>
        Assertion.And(left, right)

      case q"zio.prelude.refined.Assertion.Or.apply[$_](${unliftAssertion(left)}, ${unliftAssertion(right)})" =>
        Assertion.Or(left, right)

      case q"${unliftAssertion(left)}.||[$_](${unliftAssertion(right)})" =>
        Assertion.Or(left, right)

      //      case other =>
      //        c.abort(c.enclosingPosition, s"\nCould not unlift assertion $other")
    }

  def wrap_impl[A: c.WeakTypeTag](value: c.Expr[A]): c.Tree = {
    val tree = astTree(c.prefix.tree)
    tree match {
      case Some(q"${assertionC: Assertion[Any]}") =>
        literalUnlifter.lift(value.tree) match {
          case Some(valueC) =>
            assertionC(valueC) match {
              case Left(error) =>
                val header  =
                  scala.Console.BOLD + scala.Console.RED + scala.Console.REVERSED + "  Assertion Failed  " + scala.Console.RESET
                val message = s"""
                                 |$header
                                 |${error.render}
                                 |""".stripMargin

                c.abort(c.enclosingPosition, message)
              case Right(_)    =>
                q"${c.prefix}.unsafeApply($value)"
            }
          case None         =>
            val header  =
              scala.Console.BOLD + scala.Console.RED + scala.Console.REVERSED + "  Assertion Failed  " + scala.Console.RESET
            val message = s"""
                             |$header
                             |Could not validate Smart Assertion at compile-time.
                             |Either use a literal or call ${scala.Console.BLUE + s"${c.prefix.tree}.wrapEither(${value.tree})" + scala.Console.RESET}
                             |""".stripMargin

            c.abort(c.enclosingPosition, message)
        }

      case _ =>
        c.abort(c.enclosingPosition, "MISSING ASSERTION")
    }
  }

  private def astTree(tree: Tree): Option[c.Tree] =
    for {
      annotation <- tree.tpe.decls.flatMap(_.annotations).headOption
      astTree    <- annotation.tree.children.lastOption
    } yield astTree
}
