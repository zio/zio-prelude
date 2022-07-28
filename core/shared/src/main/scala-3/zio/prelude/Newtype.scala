package zio.prelude

import zio.NonEmptyChunk

import scala.quoted.*
import zio.prelude.ConsoleUtils.*

abstract class Newtype[A] {

  type Type
  
  def assertion: Assertion[A] = Assertion.anything

  /**
   * Derives an instance of a type class for the new type given an instance
   * of the type class for the underlying type. The caller is responsible for
   * the type class being a valid instance for the new type.
   */
  protected def derive[TypeClass[_]](implicit instance: TypeClass[A]): TypeClass[Type] =
    instance.asInstanceOf[TypeClass[Type]]

  /**
   * Allows pattern matching on newtype instances to convert them back to
   * instances of the underlying type.
   */
  def unapply(value: Type): Some[A] = Some(unwrap(value))

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype. Ignores the assertion.
   */
  protected def wrap(value: A): Type = value.asInstanceOf[Type]

  /**
   * Converts an instance of the newtype back to an instance of the
   * underlying type.
   */
  def unwrap(value: Type): A = value.asInstanceOf[A]

  /**
   * Converts an instance of a type parameterized on the newtype back to an
   * instance of a type parameterized on the underlying type. For example,
   * this could be used to convert a list of instances of the newtype back
   * to a list of instances of the underlying type.
   */

  def unwrapAll[F[_]](value: F[Type]): F[A] = value.asInstanceOf[F[A]]

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype.
   */
  inline def apply(inline value: A): Type =  
    ${Macros.make_Impl[A, Type]('assertion, 'value)}

  inline def apply(inline value: A, inline values: A*): NonEmptyChunk[Type] = 
    ${Macros.makeMany_Impl[A, Type]('{assertion}, 'value, 'values)}

  def make(value: A): Validation[String, Type] = 
    Validation.fromEitherNonEmptyChunk(
      assertion.apply(value).left.map(e => NonEmptyChunk.fromCons(e.toNel(value.toString)))
    ).as(value.asInstanceOf[Type])


  def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[Type]] =
    ForEach[F].forEach(value) { value =>
      Validation.fromEitherNonEmptyChunk(
        assertion.apply(value).left.map(e => NonEmptyChunk.fromCons(e.toNel(value.toString)))
      )
    }.as(value.asInstanceOf[F[Type]])
}

object Macros extends Liftables {

  def make_Impl[A: Type, T: Type](assertionExpr: Expr[Assertion[A]], a: Expr[A])(using Quotes): Expr[T] = {
    import quotes.reflect.*


    assertionExpr.value match {
      case Some(assertion) =>
        a match {
          case LiteralUnlift(x) =>
            assertion(x.asInstanceOf[A]) match {
              case Right(_)    => '{ $a.asInstanceOf[T] }
              case Left(error) => 
                report.errorAndAbort(s"$refinementErrorHeader\n" + error.render(x.toString))
            }

          case _               =>
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

          case Select(ident @ Ident(name), "assertion") if ident.tpe <:< TypeRepr.of[Newtype[_]]=> 
            '{ $a.asInstanceOf[T] }

          case other => 
            val source = scala.util.Try(assertionExpr.asTerm.pos.sourceCode.get).getOrElse(assertionExpr.show)
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

  def makeMany_Impl[A: Type, T: Type](assertionExpr: Expr[Assertion[A]], a: Expr[A], as: Expr[Seq[A]])(using Quotes
  ): Expr[NonEmptyChunk[T]] = {
    import quotes.reflect.*

    assertionExpr.value match {
      case Some(assertion) =>
        as match {
          case Varargs(exprs) =>
            val validated = exprs.map {
              case LiteralUnlift(x) => assertion(x.asInstanceOf[A]).left.map(_.render(x.toString))
              case _ => report.errorAndAbort(s"$refinementErrorHeader\nMust use literal value for macro.")
            }

            val (errors, _) = validated.partitionMap(identity)

            // val rendered = ${dim("assertion =")} ${assertion}

            if (errors.nonEmpty)
               report.errorAndAbort(s"""$refinementErrorHeader
${errors.mkString("\n")}
""")

            '{ NonEmptyChunk($a, $as*).asInstanceOf[NonEmptyChunk[T]] }

          case _ =>
              report.errorAndAbort(s"NO VARARGS!?")
        }
      
      case None =>
        report.errorAndAbort(s"NO REFINEMENT!?")
    }
  }

  private val refinementErrorHeader =
    s"${Console.BOLD + Console.RED + Console.REVERSED} Assertion Failed ${Console.RESET}"
}