package zio.prelude.refined

import scala.reflect.macros.whitebox

class Macros(val c: whitebox.Context) extends Liftables {
  import c.universe._

  def makeRefined[A: c.WeakTypeTag, T: c.WeakTypeTag](assertion: c.Tree): c.Tree =
    assertion match {
      case q"${assertion: Assertion[A]}" =>
        val anonTrait = TypeName(c.freshName("anonTrait"))
        q"""
          trait $anonTrait {
            @${c.weakTypeOf[QuotedAssertion]}($assertion)
            def fun: Int = ???
          }
          
          new _root_.zio.prelude.refined.Refined[$anonTrait, ${c.weakTypeOf[A]}, ${c.weakTypeOf[T]}] {
            @${c.weakTypeOf[QuotedAssertion]}($assertion)
            def assertion = $assertion
          }
        """

      case _ =>
        c.abort(c.enclosingPosition, s"FAILED TO UNLIFT ASSERTION: $assertion")
    }

  def smartApply[Meta: WeakTypeTag, A: c.WeakTypeTag, T: c.WeakTypeTag](value: c.Tree): c.Tree = {
    val assertion = c.weakTypeOf[Meta].decls.flatMap(_.annotations).headOption.flatMap(_.tree.children.lastOption)

    assertion match {
      case Some(q"${assertion: Assertion[A]}") =>
        LiteralUnlift.unapply(value) match {
          case Some(value) =>
            assertion(value.asInstanceOf[A]) match {
              case Left(error) => c.abort(c.enclosingPosition, error.render(value.toString))
              case Right(_)    => q"${c.prefix}.unsafeApply(${LiteralLift.unapply(value).get})"
            }
          case _           =>
            val message =
              s"""
                 |${Console.BOLD + Console.RED + Console.REVERSED}Assertion Failed${Console.RESET}
                 |Could not validate Smart Assertion at compile-time.
                 |Either use a literal or call ${Console.BLUE}"${c.prefix.tree}.make($value)"${Console.RESET}
                 |""".stripMargin

            c.abort(c.enclosingPosition, message)
        }

      case _ =>
        c.abort(c.enclosingPosition, s"FAILED TO UNLIFT ASSERTION: $assertion")
    }
  }

  def wrap_impl[A: c.WeakTypeTag](value: c.Expr[A]): c.Tree = {
    val assertion = astTree(c.prefix.tree)
    assertion match {
      case Some(q"${assertion: Assertion[A]}") =>
        LiteralUnlift.unapply(value.tree) match {
          case Some(value) =>
            assertion(value.asInstanceOf[A]) match {
              case Left(error) => c.abort(c.enclosingPosition, error.render(value.toString))
              case Right(_)    => q"${c.prefix}.unsafeApply(${LiteralLift.unapply(value).get})"
            }
          case _           =>
            val message =
              s"""
                 |${Console.BOLD + Console.RED + Console.REVERSED}Assertion Failed${Console.RESET}
                 |Could not validate Smart Assertion at compile-time.
                 |Either use a literal or call ${Console.BLUE}"${c.prefix.tree}.wrapEither($value)"${Console.RESET}
                 |""".stripMargin

            c.abort(c.enclosingPosition, message)
        }

      case _ =>
        c.abort(c.enclosingPosition, s"FAILED TO UNLIFT ASSERTION: $assertion")
    }
  }

  private def astTree(tree: Tree): Option[c.Tree] =
    for {
      annotation <- tree.tpe.decls.flatMap(_.annotations).headOption
      astTree    <- annotation.tree.children.lastOption
    } yield astTree
}
