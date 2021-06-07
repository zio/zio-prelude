package zio.prelude.refined

import zio.prelude.refined.macros.Macros

trait NewtypeSmart[A] { self =>
  type Type

  def assertion: Assertion[A]

  def unsafeWrap(a: A): Type = a.asInstanceOf[Type]

  def unwrap(t: Type): A = t.asInstanceOf[A]

  def wrapEither(a: A): Either[AssertionError, Type] =
    assertion.apply(a) match {
      case Left(error) => Left(error)
      case Right(_)    => Right(unsafeWrap(a))
    }

  def apply(value: A): Type =
    macro Macros.wrap_impl[A]
}

object NewtypeSmart {
  def apply[A](assertion: Assertion[A]): NewtypeSmart[A] =
    macro Macros.make_impl[A, NewtypeSmart[A]]

  implicit final class NewtypeOps[A](self: NewtypeSmart[A]#Type) {
    def value: A = self.asInstanceOf[A]
  }
}
