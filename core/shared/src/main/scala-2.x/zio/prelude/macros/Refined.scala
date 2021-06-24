package zio.prelude.macros

import zio.prelude.refined.Assertion

trait Refined[A, Meta] { self =>
  def assertion: Assertion[A]

  trait Newtype {
    type Type

    def apply(value: A): Type = macro Macros.smartApply[A, Meta]

    def make(value: A): Either[String, Type] =
      assertion(value) match {
        case Left(error) => Left(error.render(value.toString))
        case Right(_)    => Right(value.asInstanceOf[Type])
      }

    def unsafeApply(a: A): Type = a.asInstanceOf[Type]
  }

  abstract class Subtype extends Newtype {
    type Type <: A
  }
}

object Refined {
  def apply[A](assertion: Assertion[A]): Refined[A, _] = macro Macros.makeRefined[A]
}
