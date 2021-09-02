package zio.prelude

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox

final case class refinementQuote[A](refinement: Refinement[A]) extends StaticAnnotation

trait QuotedRefinement[A]

class Macros(val c: whitebox.Context) extends Liftables {
  import c.universe._

  def wrapAll_impl[F[_], A: c.WeakTypeTag, T: c.WeakTypeTag](value: c.Expr[F[A]]): c.Expr[F[T]] = {
    val expr = value

    val quotedRefinement = c.prefix.actualType.decls
      .find(_.typeSignature.resultType.widen <:< c.weakTypeOf[QuotedRefinement[_]])

    quotedRefinement match {
      case Some(_) =>
        val message =
          s"""
             |$refinementErrorHeader
             |You cannot use `wrapAll` if you have a refinement:
             |${show(quotedRefinement)}
             |""".stripMargin
        c.abort(c.enclosingPosition, message)
      case None    =>
        c.Expr[F[T]](q"_root.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, $expr)")
    }
  }

  def wrapAllVarargs_impl[A: c.WeakTypeTag, T: c.WeakTypeTag](values: c.Expr[A]*): c.Expr[List[T]] = {
    val quotedRefinement = c.prefix.actualType.decls
      .find(_.typeSignature.resultType.widen <:< c.weakTypeOf[QuotedRefinement[_]])

    quotedRefinement match {
      case Some(quotedRefinement) =>
        val refinement = getRefinement[T, A](quotedRefinement)

        val (errors, _) = values.partitionMap { value =>
          value.tree match {
            case Literal(Constant(value)) =>
              refinement(value.asInstanceOf[A]) match {
                case Left(error) => Left(error.render(value.toString))
                case Right(_)    => Right(())
              }
            case _                        =>
              val message = s"""
                               |$refinementErrorHeader
                               |Could not validate Smart Refinement at compile-time.
                               |Either use a literal or call ${Console.BLUE}"${c.prefix.tree}.unsafeWrapAll($values)"${Console.RESET}
                               |""".stripMargin

              c.abort(c.enclosingPosition, message)
          }
        }

        if (errors.nonEmpty) {
          val message = s"""
                           |$refinementErrorHeader
                           |${errors.mkString("\n")}
                           |""".stripMargin

          c.abort(c.enclosingPosition, message)
        } else {
          c.Expr[List[T]](q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, List(..$values))")
        }

      case None =>
        c.Expr[List[T]](q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, List(..$values))")
    }
  }

  def wrap_impl[A: c.WeakTypeTag, T: c.WeakTypeTag](value: c.Expr[A]): c.Expr[T] = {
    val expr = value

    val quotedRefinement = c.prefix.actualType.decls
      .find(_.typeSignature.resultType.widen <:< c.weakTypeOf[QuotedRefinement[_]])

    quotedRefinement match {
      case Some(quotedRefinement) =>
        expr.tree match {
          case Literal(Constant(value)) =>
            val refinement = getRefinement[T, A](quotedRefinement)

            refinement.apply(value.asInstanceOf[A]) match {
              case Left(error) =>
                val message =
                  s"""
                     |$refinementErrorHeader
                     |${error.render(value.toString)}
                     |""".stripMargin

                c.abort(c.enclosingPosition, message)

              case Right(_) =>
                c.Expr[T](q"_root_.zio.prelude.Newtype.unsafeWrap(${c.prefix}, $expr)")
            }

          case _ =>
            val message =
              s"""
                 |$refinementErrorHeader
                 |Could not validate Smart Refinement at compile-time.
                 |Either use a literal or call ${Console.BLUE}"${c.prefix.tree}.unsafeWrap($expr)"${Console.RESET}
                 |""".stripMargin

            c.abort(c.enclosingPosition, message)

        }

      case None =>
        c.Expr[T](q"_root_.zio.prelude.Newtype.unsafeWrap(${c.prefix}, $expr)")
    }

  }

  def refine_impl[A: c.WeakTypeTag](refinement: c.Tree): c.Tree =
    q"""
new _root_.zio.prelude.QuotedRefinement[${c.weakTypeOf[A]}] {
  @_root_.zio.prelude.refinementQuote($refinement)
  def refinement = $refinement
}
       """

  private def getRefinement[T: c.WeakTypeTag, A: c.WeakTypeTag](quotedRefinement: c.universe.Symbol): Refinement[A] = {
    val maybeRefinement = quotedRefinement.typeSignature.resultType.decls
      .flatMap(_.annotations)
      .flatMap(_.tree.children.lastOption)
      .headOption
    maybeRefinement match {
      case Some(q"${refinement: Refinement[A]}") =>
        refinement
      case _                                     =>
        c.abort(c.enclosingPosition, s"FAILED TO UNLIFT REFINEMENT: $quotedRefinement")
    }
  }

  private val refinementErrorHeader =
    s"${Console.BOLD + Console.RED + Console.REVERSED} Refinement Failed ${Console.RESET}"
}
