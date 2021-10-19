package zio.prelude

import zio.NonEmptyChunk

import scala.quoted.*
import zio.prelude.ConsoleUtils.*

trait NewtypeCompanionVersionSpecific


trait NewtypeVersionSpecific[A] { self: NewtypeModule#Newtype[A] =>

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype.
   */
  inline def apply(inline value: A): Type =  
    ${Macros.make_Impl[A, Type]('{assertion}, 'value)}

  inline def apply(inline value: A, inline values: A*): NonEmptyChunk[Type] = 
    ${Macros.makeMany_Impl[A, Type]('{assertion}, 'value, 'values)}
  
  def assertion: Assertion[A] = Assertion.anything

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

  def make_Impl[A: Type, T: Type](refinementExpr: Expr[Assertion[A]], a: Expr[A])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    refinementExpr.value match {
      case Some(assertion) =>
        a match {
          case LiteralUnlift(x) =>
            assertion(x.asInstanceOf[A]) match {
              case Right(_)    => '{ $a.asInstanceOf[T] }
              case Left(error) => report.throwError(s"$refinementErrorHeader\n" + error.render(x.toString))
            }
          case _               =>
            report.throwError(s"$refinementErrorHeader\nMust use literal value for macro.")
        }

      case None =>
        '{ $a.asInstanceOf[T] }
    }
  }

  def makeMany_Impl[A: Type, T: Type](refinementExpr: Expr[Assertion[A]], a: Expr[A], as: Expr[Seq[A]])(using Quotes
  ): Expr[NonEmptyChunk[T]] = {
    import quotes.reflect.*

    refinementExpr.value match {
      case Some(assertion) =>
        as match {
          case Varargs(exprs) =>
            val validated = exprs.map {
              case LiteralUnlift(x) => assertion(x.asInstanceOf[A]).left.map(_.render(x.toString))
              case _ => report.throwError(s"$refinementErrorHeader\nMust use literal value for macro.")
            }

            val (errors, _) = validated.partitionMap(identity)

            // val rendered = ${dim("assertion =")} ${assertion}

            if (errors.nonEmpty)
               report.throwError(s"""$refinementErrorHeader
${errors.mkString("\n")}
""")

            '{ NonEmptyChunk($a, $as*).asInstanceOf[NonEmptyChunk[T]] }

          case _ =>
              report.throwError(s"NO VARARGS!?")
        }
      
      case None =>
        report.throwError(s"NO REFINEMENT!?")
    }
  }

  private val refinementErrorHeader =
    s"${Console.BOLD + Console.RED + Console.REVERSED} Assertion Failed ${Console.RESET}"
}