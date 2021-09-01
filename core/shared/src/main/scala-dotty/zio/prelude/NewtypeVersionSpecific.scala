package zio.prelude

import zio.prelude.refined.Refinement

trait NewtypeVersionSpecific[A] { self: NewtypeModule#Newtype[A] =>

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype.
   */
  inline def apply(inline value: A): Type =  
    ${Macros.makeImpl[A, Type]('{refinement}, 'value)}
  
  def refinement: Refinement[A] = Refinement.always

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype.
   */
  inline def wrap(inline value: A): Type = 
    ${Macros.makeImpl[A, Type]('refinement, 'value)}

  /**
   * Converts an instance of a type parameterized on the underlying type
   * to an instance of a type parameterized on the newtype. For example,
   * this could be used to convert a list of instances of the underlying
   * type to a list of instances of the newtype.
   */
  // def wrapAll[F[_]](value: F[A]): F[Type] = macro zio.prelude.refined.Macros.wrapAll_impl[F, A, Type]

  // def wrapAll[F[_]](values: A*): List[Type] = macro zio.prelude.refined.Macros.wrapAllVarargs_impl[A, Type]

}

import scala.quoted.*

object Macros extends refined.Liftables {
  def box(proxy: Expr[Any])(using Quotes) = {
    import quotes.reflect._

    println(proxy.asTerm)

    throw new Error("OH")
  }

  def makeImpl[A: Type, T: Type](refinementExpr: Expr[Refinement[A]], a: Expr[A])(using Quotes
  ): Expr[T] = {
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

    private val refinementErrorHeader =
    s"${Console.BOLD + Console.RED + Console.REVERSED} Refinement Failed ${Console.RESET}"
}