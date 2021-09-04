package zio.prelude

import zio.NonEmptyChunk
import zio.prelude.ConsoleUtils._

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox

final case class refinementQuote[A](refinement: Refinement[A]) extends StaticAnnotation
final case class refinementString(string: String)              extends StaticAnnotation

trait QuotedRefinement[A] {
  def refinement: Refinement[A]
}

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
        c.Expr[F[T]](q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, $expr)")
    }
  }

  def applyMany_impl[A: c.WeakTypeTag, T: c.WeakTypeTag](
    value: c.Expr[A],
    values: c.Expr[A]*
  ): c.Expr[NonEmptyChunk[T]] = {
    val quotedRefinement = c.prefix.actualType.decls
      .find(_.typeSignature.resultType.widen <:< c.weakTypeOf[QuotedRefinement[_]])

    val allValues = value +: values

    quotedRefinement match {
      case Some(quotedRefinement) =>
        val (refinement, code) = getRefinement[T, A](quotedRefinement)

        val errors = allValues.flatMap { value =>
          value.tree match {
            case Literal(Constant(value)) =>
              refinement(value.asInstanceOf[A]) match {
                case Left(error) => Some(error.render(value.toString))
                case Right(_)    => None
              }
            case _                        =>
              val message = s"""
                               |$refinementErrorHeader
                               |Could not validate Refinement at compile-time.
                               |Either use a literal or call ${Console.BLUE}"${c.prefix.tree}.unsafeWrapAll(List($value, ..$values))"${Console.RESET}
                               |""".stripMargin

              c.abort(c.enclosingPosition, message)
          }
        }

        if (errors.nonEmpty) {
          val message = s"""
                           |$refinementErrorHeader
                           |${"\u001b[2m"}refinement = ${Console.RESET + Console.YELLOW}$code${Console.RESET}
                           |
                           |${errors.mkString("\n")}
                           |""".stripMargin

          c.abort(c.enclosingPosition, message)
        } else {
          c.Expr[NonEmptyChunk[T]](
            q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, _root_.zio.NonEmptyChunk($value, ..$values))"
          )
        }

      case None =>
        c.Expr[NonEmptyChunk[T]](
          q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, _root_.zio.NonEmptyChunk($value, ..$values))"
        )
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
            val (refinement, code) = getRefinement[T, A](quotedRefinement)

            refinement.apply(value.asInstanceOf[A]) match {
              case Left(error) =>
                val message =
                  s"""
                     |$refinementErrorHeader
                     |${"\u001b[2m"}refinement = ${Console.RESET + Console.YELLOW}$code${Console.RESET}
                     |
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
                 |Could not validate Refinement at compile-time.
                 |Either use a literal or call ${Console.BLUE}"${c.prefix.tree}.unsafeWrap($expr)"${Console.RESET}
                 |""".stripMargin

            c.abort(c.enclosingPosition, message)

        }

      case None =>
        c.Expr[T](q"_root_.zio.prelude.Newtype.unsafeWrap(${c.prefix}, $expr)")
    }

  }

  def make_impl[A: c.WeakTypeTag, T: c.WeakTypeTag](value: c.Expr[A]): c.Tree = {
    val expr = value

    val quotedRefinement = c.prefix.actualType.decls
      .find(_.typeSignature.resultType.widen <:< c.weakTypeOf[QuotedRefinement[_]])

    quotedRefinement match {
      case Some(quotedRefinement) =>
        val result = q"_root_.zio.prelude.Newtype.unsafeWrap(${c.prefix}, $expr)"
        q"""
_root_.zio.prelude.Validation.fromEitherNonEmptyChunk {
  $quotedRefinement.refinement.apply($value)
    .left.map(_.toNonEmptyChunk($value.toString))
}.as($result)
"""

      case None =>
        val result = q"_root_.zio.prelude.Newtype.unsafeWrap(${c.prefix}, $expr)"
        q"_root_.zio.prelude.Validation.succeed($result)"
    }

  }

  def makeAll_impl[F[+_], A: c.WeakTypeTag, T: c.WeakTypeTag](
    value: c.Expr[F[A]]
  )(forall: c.Tree): c.Tree = {
    val expr = value
    val _    = forall

    val quotedRefinement = c.prefix.actualType.decls
      .find(_.typeSignature.resultType.widen <:< c.weakTypeOf[QuotedRefinement[_]])

    quotedRefinement match {
      case Some(quotedRefinement) =>
        val result = q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, $expr)"
        q"""
$forall.forEach($value) { value =>
  _root_.zio.prelude.Validation.fromEitherNonEmptyChunk {
    $quotedRefinement.refinement.apply(value)
      .left.map(_.toNonEmptyChunk(value.toString))
  }
}.as($result)

"""

      case None =>
        val result = q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, $expr)"
        q"_root_.zio.prelude.Validation.succeed($result)"
    }

  }

  def refine_impl[A: c.WeakTypeTag](refinement: c.Tree): c.Tree = {
    val (_, _, codeString) = text(refinement)
    q"""
new _root_.zio.prelude.QuotedRefinement[${c.weakTypeOf[A]}] {
  @_root_.zio.prelude.refinementQuote($refinement)
  @_root_.zio.prelude.refinementString($codeString)
  def magic = 42
  
  def refinement = $refinement
}
       """
  }

  private def getRefinement[T: c.WeakTypeTag, A: c.WeakTypeTag](
    quotedRefinement: c.universe.Symbol
  ): (Refinement[A], String) = {
    val trees = quotedRefinement.typeSignature.resultType.decls
      .flatMap(_.annotations)
      .flatMap(_.tree.children.lastOption)

    val maybeRefinement: Option[Refinement[A]] = trees.collectFirst { case q"${refinement: Refinement[A]}" =>
      refinement
    }

    val maybeCodeString: Option[String] = trees.collectFirst { case q"${string: String}" =>
      string
    }

    (maybeRefinement, maybeCodeString) match {
      case (Some(refinement), Some(code)) =>
        (refinement, code)
      case _                              =>
        val signatureExample =
          yellow("def refinement: ") + underlined(yellow(s"QuotedAssertion[${weakTypeOf[A]}]")) +
            yellow(" = refine(...)")
        val message          =
          s"""
             |$refinementErrorHeader
             |We were unable to read your refinement at compile-time.
             |This could be for one of two reasons:
             |
             | ${red("1.")} You have annotated `def refinement` with its type signature.
             |      $signatureExample
             |    
             |    Due to the macro machinery powering this feature, you ${red("MUST NOT ANNOTATE")} this method. 
             |    ${underlined("Try deleting the type annotation and recompiling.")}
             |    
             | ${red("2.")} You have defined your Refinement in a way that cannot be read at compile-time.
             |    Due to the limitations of macros, refinements cannot be abstracted into other definitions.
             |    Make certain your definition looks something like this:
             |      ${yellow("import zio.prelude.Refinement._")}
             |      ${yellow("def refinement = refine(greaterThan(40) && lessThan(80))")}
             |      
             |""".stripMargin
        c.abort(c.enclosingPosition, message)
    }
  }

  private val refinementErrorHeader =
    s"${Console.BOLD + Console.RED + Console.REVERSED} Newtype Refinement Failed ${Console.RESET}"

  // Pilfered (with immense gratitude & minor modifications)
  // from https://github.com/com-lihaoyi/sourcecode
  private def text[T: c.WeakTypeTag](tree: c.Tree): (Int, Int, String) = {
    val fileContent  = new String(tree.pos.source.content)
    var start        = tree.collect { case treeVal =>
      treeVal.pos match {
        case NoPosition => Int.MaxValue
        case p          => p.start
      }
    }.min
    val initialStart = start

    // Moves to the true beginning of the expression, in the case where the
    // internal expression is wrapped in parens.
    while ((start - 2) >= 0 && fileContent(start - 2) == '(')
      start -= 1

    val g      = c.asInstanceOf[reflect.macros.runtime.Context].global
    val parser = g.newUnitParser(fileContent.drop(start))
    parser.expr()
    val end    = parser.in.lastOffset
    (initialStart - start, start, fileContent.slice(start, start + end))
  }

}
