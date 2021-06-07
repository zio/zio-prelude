package zio.prelude.refined.macros

import zio.prelude.refined.{Assertion, NewtypeSmart}

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox

case class QuotedAssertion(assertion: Assertion[_]) extends StaticAnnotation

class Macros(val c: whitebox.Context) {
  import c.universe.{TypeTag => _, _}
  val AssertionM = q"_root_.refined.Assertion"

  val literalUnlifter: PartialFunction[Tree, Any] = {
    case q"${int: Int}"                                     => int
    case q"${string: String}"                               => string
    case q"${boolean: Boolean}"                             => boolean
    case q"scala.Some.apply[$_](${literalUnlifter(value)})" => Some(value)
    case q"scala.None"                                      => None
  }

  def orderingForValue(any: Any): Option[Ordering[Any]] = any match {
    case _: Int    => Some(scala.Ordering.Int.asInstanceOf[Ordering[Any]])
    case _: String => Some(scala.Ordering.String.asInstanceOf[Ordering[Any]])
    case ord       => c.abort(c.enclosingPosition, s"NO ORDERING FOR $ord")
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
      case Assertion.EqualTo(literalLifter(value))     =>
        q"$AssertionM.EqualTo($value)"
      case matches: Assertion.Matches                  =>
        q"$AssertionM.Matches(${matches.regex})"
      case Assertion.GreaterThan(literalLifter(value)) =>
        q"$AssertionM.GreaterThan($value)"
      case Assertion.LessThan(literalLifter(value))    =>
        q"$AssertionM.LessThan($value)"
      case Assertion.And(lhs, rhs)                     =>
        q"$AssertionM.And($lhs, $rhs)"
      case _                                           =>
        c.abort(c.enclosingPosition, s"FAILED TO LIFT ASSERTION")
    }

  implicit lazy val unliftAssertion: Unliftable[Assertion[Any]] =
    Unliftable[Assertion[Any]] {
      case q"refined.Assertion.EqualTo.apply[$_](${literalUnlifter(value)})" =>
        Assertion.EqualTo(value)

      case q"refined.Assertion.equalTo[$_](${literalUnlifter(value)})" =>
        Assertion.EqualTo(value)

      case q"refined.Assertion.lessThan[$_](${literalUnlifter(value)})($_)" =>
        Assertion.LessThan(value)(orderingForValue(value).get)

      case q"refined.Assertion.LessThan.apply[$_](${literalUnlifter(value)})($_)" =>
        Assertion.LessThan(value)(orderingForValue(value).get)

      case q"refined.Assertion.greaterThan[$_](${literalUnlifter(value)})($_)" =>
        Assertion.GreaterThan(value)(orderingForValue(value).get)

      case q"refined.Assertion.GreaterThan.apply[$_](${literalUnlifter(value)})($_)" =>
        Assertion.GreaterThan(value)(orderingForValue(value).get)

      case q"refined.Assertion.matches(${regex: String})" =>
        Assertion.Matches(regex).asInstanceOf[Assertion[Any]]

      case q"refined.Assertion.Matches.apply(${regex: String})" =>
        Assertion.Matches(regex).asInstanceOf[Assertion[Any]]

      case q"refined.Assertion.And.apply[$_](${unliftAssertion(left)}, ${unliftAssertion(right)})" =>
        Assertion.And(left, right)

      case q"${unliftAssertion(left)}.&&[$_](${unliftAssertion(right)})" =>
        Assertion.And(left, right)

//      case other =>
//        c.abort(c.enclosingPosition, s"\nCould not unlift assertion $other")
    }

  def make_impl[A: WeakTypeTag, F: c.WeakTypeTag](assertion: c.Tree): c.Tree =
    assertion match {
      case q"${assertionC: Assertion[Any]}" =>
        val newtypeOrSubtype =
          if (c.weakTypeOf[F].typeConstructor =:= c.weakTypeOf[NewtypeSmart[_]].typeConstructor)
            tq"_root_.refined.NewtypeSmart[${c.weakTypeOf[A]}]"
          else tq"_root_.refined.SubtypeSmart[${c.weakTypeOf[A]}]"

        c.untypecheck {
          q"""
new $newtypeOrSubtype {
  @${c.weakTypeOf[QuotedAssertion]}($assertionC)
  def assertion = $assertionC
}
             """
        }

      case _ =>
        c.abort(c.enclosingPosition, s"FAILED TO UNLIFT ASSERTION: $assertion")
    }

  def wrap_impl[A: c.WeakTypeTag](value: c.Expr[A]): c.Tree = {
    val tree = astTree(c.prefix.tree)
    tree match {
      case Some(q"${assertionC: Assertion[Any]}") =>
        literalUnlifter.unapply(value.tree) match {
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
                q"${c.prefix}.unsafeWrap($value)"
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
