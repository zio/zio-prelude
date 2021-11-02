package zio.prelude

import com.github.ghik.silencer.silent
import zio.prelude.ConsoleUtils._

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox

final case class assertionQuote[A](assertion: Assertion[A]) extends StaticAnnotation
final case class assertionString(string: String)            extends StaticAnnotation

trait QuotedAssertion[A] {
  def assertion: Assertion[A]
}

// Wrongly emits warnings on Scala 2.12.x https://github.com/scala/bug/issues/11918
@silent("pattern var .* in method unapply is never used: use a wildcard `_` or suppress this warning with .*")
private[prelude] class Macros(val c: whitebox.Context) extends Liftables {
  import c.universe._

  def wrapAll_impl[F[_], A: c.WeakTypeTag, T: c.WeakTypeTag](value: c.Expr[F[A]]): c.Expr[F[T]] = {
    val expr = value

    val quotedAssertion = c.prefix.actualType.decls
      .find(_.typeSignature.resultType.widen <:< c.weakTypeOf[QuotedAssertion[_]])

    quotedAssertion match {
      case Some(_) =>
        val message =
          s"""
             |$assertionErrorHeader
             |You cannot use `wrapAll` if you have a assertion:
             |${show(quotedAssertion)}
             |""".stripMargin
        c.abort(c.enclosingPosition, message)
      case None    =>
        c.Expr[F[T]](q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, $expr)")
    }
  }

  def applyMany_impl[A: c.WeakTypeTag, T: c.WeakTypeTag](
    value: c.Expr[A],
    values: c.Expr[A]*
  ): c.Tree = {
    val quotedAssertion = c.prefix.actualType.decls
      .find(_.typeSignature.resultType.widen <:< c.weakTypeOf[QuotedAssertion[_]])

    val allValues = value +: values

    quotedAssertion match {
      case Some(quotedAssertion) =>
        val (assertion, code) = getAssertion[T, A](quotedAssertion)

        val errors = allValues.flatMap { value =>
          value.tree match {
            case Literal(Constant(value)) =>
              assertion(value.asInstanceOf[A]) match {
                case Left(error) => Some(error.render(value.toString))
                case Right(_)    => None
              }
            case _                        =>
              val message = s"""
                               |$assertionErrorHeader
                               |Could not validate Assertion at compile-time.
                               |Either use a literal or call ${Console.BLUE}"${c.prefix.tree}.unsafeWrapAll(List($value, ..$values))"${Console.RESET}
                               |""".stripMargin

              c.abort(c.enclosingPosition, message)
          }
        }

        if (errors.nonEmpty) {
          val message = s"""
                           |$assertionErrorHeader
                           |${"\u001b[2m"}assertion = ${Console.RESET + Console.YELLOW}$code${Console.RESET}
                           |
                           |${errors.mkString("\n")}
                           |""".stripMargin

          c.abort(c.enclosingPosition, message)
        } else {

          q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, _root_.zio.NonEmptyChunk($value, ..$values))"
        }

      case None =>
        q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, _root_.zio.NonEmptyChunk($value, ..$values))"
    }
  }

  def wrap_impl[A: c.WeakTypeTag, T: c.WeakTypeTag](value: c.Expr[A]): c.Expr[T] = {
    val expr = value

    val quotedAssertion = c.prefix.actualType.decls
      .find(_.typeSignature.resultType.widen <:< c.weakTypeOf[QuotedAssertion[_]])

    quotedAssertion match {
      case Some(quotedAssertion) =>
        expr.tree match {
          case Literal(Constant(value)) =>
            val (assertion, code) = getAssertion[T, A](quotedAssertion)

            assertion.apply(value.asInstanceOf[A]) match {
              case Left(error) =>
                val message =
                  s"""
                     |$assertionErrorHeader
                     |${"\u001b[2m"}assertion = ${Console.RESET + Console.YELLOW}$code${Console.RESET}
                     |
                     |${error.render(value.toString)}
                     |""".stripMargin

                c.abort(c.enclosingPosition, message)

              case Right(_) =>
                c.Expr[T](q"_root_.zio.prelude.Newtype.unsafeWrap(${c.prefix})($expr)")
            }

          case _ =>
            val message =
              s"""
                 |$assertionErrorHeader
                 |Could not validate Assertion at compile-time.
                 |Either use a literal or call ${Console.BLUE}"${c.prefix.tree}.unsafeWrap($expr)"${Console.RESET}
                 |""".stripMargin

            c.abort(c.enclosingPosition, message)

        }

      case None =>
        c.Expr[T](q"_root_.zio.prelude.Newtype.unsafeWrap(${c.prefix})($expr)")
    }

  }

  def make_impl[A: c.WeakTypeTag, T: c.WeakTypeTag](value: c.Expr[A]): c.Tree = {
    val expr = value

    val result = q"_root_.zio.prelude.Newtype.unsafeWrap(${c.prefix})($expr)"

    q"""
_root_.zio.prelude.Validation.fromEitherNonEmptyChunk {
  ${c.prefix}.assertion.assertion.apply($value)
    .left.map(e => _root_.zio.NonEmptyChunk.fromCons(e.toNel($value.toString)))
}.as($result)
"""

  }

  def makeAll_impl[F[+_], A: c.WeakTypeTag, T: c.WeakTypeTag](
    value: c.Expr[F[A]]
  )(forall: c.Tree): c.Tree = {
    val expr = value
    val _    = forall

    val quotedAssertion = c.prefix.actualType.decls
      .find(_.typeSignature.resultType.widen <:< c.weakTypeOf[QuotedAssertion[_]])

    quotedAssertion match {
      case Some(quotedAssertion) =>
        val result = q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, $expr)"
        q"""
$forall.forEach($value) { value =>
  _root_.zio.prelude.Validation.fromEitherNonEmptyChunk {
    $quotedAssertion.assertion.apply(value)
      .left.map(e => _root_.zio.NonEmptyChunk.fromCons(e.toNel(value.toString)))
  }
}.as($result)

"""

      case None =>
        val result = q"_root_.zio.prelude.Newtype.unsafeWrapAll(${c.prefix}, $expr)"
        q"_root_.zio.prelude.Validation.succeed($result)"
    }

  }

  def refine_impl[A: c.WeakTypeTag](assertion: c.Tree): c.Tree = {
    val (_, _, codeString) = text(assertion)
    q"""
new _root_.zio.prelude.QuotedAssertion[${c.weakTypeOf[A]}] {
  @_root_.zio.prelude.assertionQuote($assertion)
  @_root_.zio.prelude.assertionString($codeString)
  def magic = 42
  
  def assertion = $assertion
}
       """
  }

  private def getAssertion[T: c.WeakTypeTag, A: c.WeakTypeTag](
    quotedAssertion: c.universe.Symbol
  ): (Assertion[A], String) = {
    val trees = quotedAssertion.typeSignature.resultType.decls
      .flatMap(_.annotations)
      .flatMap(_.tree.children.lastOption)

    val maybeAssertion: Option[Assertion[A]] = trees.collectFirst { case q"${assertion: Assertion[A]}" =>
      assertion
    }

    val maybeCodeString: Option[String] = trees.collectFirst { case q"${string: String}" =>
      string
    }

    (maybeAssertion, maybeCodeString) match {
      case (Some(assertion), Some(code)) =>
        (assertion, code)
      case _                             =>
        val signatureExample =
          yellow("def assertion: ") + underlined(yellow(s"QuotedAssertion[${weakTypeOf[A]}]")) +
            yellow(" = assert(...)")
        val message          =
          s"""
             |$assertionErrorHeader
             |We were unable to read your assertion at compile-time.
             |This could be for one of two reasons:
             |
             | ${red("1.")} You have annotated `def assertion` with its type signature.
             |      $signatureExample
             |   
             |    Due to the macro machinery powering this feature, you ${red("MUST NOT ANNOTATE")} this method. 
             |    ${underlined("Try deleting the type annotation and recompiling.")}
             |    
             | ${red("2.")} You have defined your Assertion in a way that cannot be read at compile-time.
             |    Due to the limitations of macros, assertions cannot be abstracted into other definitions.
             |    Make certain your definition looks something like this:
             |      ${yellow("import zio.prelude.Assertion._")}
             |      ${yellow("def assertion = assert(greaterThan(40) && lessThan(80))")}
             |      
             |""".stripMargin
        c.abort(c.enclosingPosition, message)
    }
  }

  private val assertionErrorHeader =
    s"${Console.BOLD + Console.RED + Console.REVERSED} Newtype Assertion Failed ${Console.RESET}"

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
