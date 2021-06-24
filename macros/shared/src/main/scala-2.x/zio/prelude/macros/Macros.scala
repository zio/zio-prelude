package zio.prelude.macros

import zio.prelude.refined._
import zio.prelude.refined.macros.QuotedAssertion

import scala.reflect.macros.whitebox

class Macros(val c: whitebox.Context) extends Liftables {
  import c.universe._

  def makeRefined[A: c.WeakTypeTag](assertion: c.Tree): c.Tree =
    assertion match {
      case q"${assertion: Assertion[A]}" =>
        val anonTrait = TypeName(c.freshName("anonTrait"))
        q"""
          trait $anonTrait {
            @${c.weakTypeOf[QuotedAssertion]}($assertion)
            def fun: Int = ???
          }
          
          new _root_.zio.prelude.macros.Refined[${c.weakTypeOf[A]}, $anonTrait] {
            @${c.weakTypeOf[QuotedAssertion]}($assertion)
            def assertion = $assertion
          }
        """

      case _ =>
        c.abort(c.enclosingPosition, s"FAILED TO UNLIFT ASSERTION: $assertion")
    }

  def smartApply[A: c.WeakTypeTag, Meta: WeakTypeTag](value: c.Tree): c.Tree = {
    val assertion = c.weakTypeOf[Meta].decls.flatMap(_.annotations).headOption.flatMap(_.tree.children.lastOption)

    (assertion, value) match {
      case (Some(q"${assertion: Assertion[A]}"), q"${LiteralUnlift(value)}") =>
        assertion(value.asInstanceOf[A]) match {
          case Left(error) => c.abort(c.enclosingPosition, error.render(value.toString))
          case Right(_)    => q"${c.prefix}.unsafeApply(${LiteralLift.unapply(value).get})"
        }

      case (Some(q"${_: Assertion[A]}"), _) =>
        val message =
          s"""
             |${Console.BOLD + Console.RED + Console.REVERSED}Assertion Failed${Console.RESET}
             |Could not validate Smart Assertion at compile-time.
             |Either use a literal or call ${Console.BLUE}"${c.prefix.tree}.wrapEither($value)"${Console.RESET}
             |""".stripMargin

        c.abort(c.enclosingPosition, message)

      case _ =>
        c.abort(c.enclosingPosition, s"FAILED TO UNLIFT ASSERTION: $assertion")
    }
  }

  def wrap_impl[A: c.WeakTypeTag](valueTree: c.Expr[A]): c.Tree = {
    val assertionTree = astTree(c.prefix.tree)
    (assertionTree, valueTree) match {
      case (Some(q"${assertion: Assertion[A]}"), q"${LiteralUnlift(value)}") =>
        assertion(value.asInstanceOf[A]) match {
          case Left(error) => c.abort(c.enclosingPosition, error.render(value.toString))
          case Right(_)    => q"${c.prefix}.unsafeApply(${LiteralLift.unapply(value).get})"
        }

      case (Some(q"${_: Assertion[A]}"), _) =>
        val message =
          s"""
             |${Console.BOLD + Console.RED + Console.REVERSED}Assertion Failed${Console.RESET}
             |Could not validate Smart Assertion at compile-time.
             |Either use a literal or call ${Console.BLUE}"${c.prefix.tree}.wrapEither($valueTree)"${Console.RESET}
             |""".stripMargin

        c.abort(c.enclosingPosition, message)

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
