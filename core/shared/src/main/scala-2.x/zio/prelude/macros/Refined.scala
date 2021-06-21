package zio.prelude.macros

import zio.prelude.Validation
import zio.prelude.refined.Assertion

trait Refined[A, Meta] { self =>
  def assertion: Assertion[A]

  trait Newtype {
    type Type

//    def apply(value: A): Type = unsafeApply(value)
    def apply(value: A): Type = macro Macros.smartApply[A, Meta]

    def make(value: A): Validation[String, A] =
      assertion(value) match {
        case Left(error) => Validation.fail(error.render)
        case Right(_) => Validation.succeed(value)
      }

    def unsafeApply(a: A): Type = a.asInstanceOf[Type]
  }

  abstract class Subtype extends Newtype {
   type Type <: A
  }
}

object Refined {
//  def apply[A](assertion0: Assertion[A]): Refined[A, _] = new Refined[A, Any] {
//    override def assertion: Assertion[A] = assertion0
//  }
  def apply[A](assertion: Assertion[A]): Refined[A, _] = macro Macros.makeRefined[A]
}

