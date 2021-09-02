package zio.prelude

import zio.NonEmptyChunk

import scala.quoted.*

trait NewtypeCompanionVersionSpecific


trait NewtypeVersionSpecific[A] { self: NewtypeModule#Newtype[A] =>

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype.
   */
  inline def apply(inline value: A): Type =  
    ${Macros.make_Impl[A, Type]('{refinement}, 'value)}

  inline def apply(inline value: A, inline values: A*): NonEmptyChunk[Type] = 
    ${Macros.makeMany_Impl[A, Type]('{refinement}, 'value, 'values)}
  
  def refinement: Refinement[A] = Refinement.always

  def make(value: A): Validation[String, Type] = 
    Validation.fromEitherNonEmptyChunk(
      refinement.apply(value).left.map(_.toNonEmptyChunk(value.toString))
    ).as(value.asInstanceOf[Type])

}

object Macros extends Liftables {

  def make_Impl[A: Type, T: Type](refinementExpr: Expr[Refinement[A]], a: Expr[A])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    refinementExpr.value match {
      case Some(refinement) =>
        a match {
          case LiteralUnlift(x) =>
            refinement(x.asInstanceOf[A]) match {
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

  def makeMany_Impl[A: Type, T: Type](refinementExpr: Expr[Refinement[A]], a: Expr[A], as: Expr[Seq[A]])(using Quotes
  ): Expr[NonEmptyChunk[T]] = {
    import quotes.reflect.*

    refinementExpr.value match {
      case Some(refinement) =>
        as match {
          case Varargs(exprs) =>
            val validated = exprs.map {
              case LiteralUnlift(x) => refinement(x.asInstanceOf[A])
              case _ => report.throwError(s"$refinementErrorHeader\nMust use literal value for macro.")
            }

            val (errors, _) = validated.partitionMap(identity)

            if (errors.nonEmpty)
               report.throwError(s"YOU FAILED ${errors}")

            '{ NonEmptyChunk($a, $as*).asInstanceOf[NonEmptyChunk[T]] }

          case _ =>
              report.throwError(s"NO VARARGS!?")
        }
      
      case None =>
        report.throwError(s"NO REFINEMENT!?")
    }
  }

  private val refinementErrorHeader =
    s"${Console.BOLD + Console.RED + Console.REVERSED} Refinement Failed ${Console.RESET}"
}