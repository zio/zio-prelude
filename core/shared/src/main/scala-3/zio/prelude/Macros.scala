package zio.prelude

import scala.quoted.*
import zio.prelude.ConsoleUtils.*

object Macros extends Liftables {

  def validateInlineImpl[A: Type](assertionExpr: Expr[Assertion[A]], a: Expr[A])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    assertionExpr.value match {
      case Some(assertion) =>
        a match {
          case LiteralUnlift(x) =>
            assertion(x.asInstanceOf[A]) match {
              case Right(_)    => '{ () }
              case Left(error) =>
                report.errorAndAbort(s"$refinementErrorHeader\n" + error.render(x.toString))
            }

          case _ =>
            report.errorAndAbort(s"$refinementErrorHeader\nMust use literal value for macro.")
        }

      case None =>
        assertionExpr.asTerm.underlying match {
          // The user forgot to use the `inline` keyword
          case Select(ident @ Ident(name), _) if ident.symbol.declarations.find(_.name == "assertion").exists { expr =>
                expr.flags.is(Flags.Override)
              } =>
            val message = s"""$refinementErrorHeader

We were unable to read your ${magenta(name)} assertion at compile-time.
You must annotate ${yellow("def assertion")} with the ${yellow("inline")} keyword:

    ${yellow("override ") + yellow(underlined("inline")) + yellow(" def assertion = ???")}.
            """
            report.errorAndAbort(message)

          case Select(ident @ Ident(name), "assertion") if ident.tpe <:< TypeRepr.of[Newtype[_]] =>
            '{ () }

          case other =>
            val source  = scala.util.Try(assertionExpr.asTerm.pos.sourceCode.get).getOrElse(assertionExpr.show)
            val message = s"""$refinementErrorHeader

We were unable to read your assertion at compile-time.

You must either define your assertion directly or refer to other inline definitions:

    ${yellow("override inline def assertion = greaterThan(10) && lessThan(100)")}.

    or

    ${yellow("inline def extracted = greaterThan(10) && lessThan(100)")}.
    ${yellow("override inline def assertion = extracted")}.
            """
            report.errorAndAbort(message)
        }
    }
  }

  private val refinementErrorHeader =
    s"${Console.BOLD + Console.RED + Console.REVERSED} Assertion Failed ${Console.RESET}"
}
