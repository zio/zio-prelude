package zio.prelude.refined

trait Refined[Meta, A, T] { self =>
  def assertion: Assertion[A]

  def apply(value: A): T = macro Macros.smartApply[Meta, A, T]

  def make(value: A): Either[String, T] =
    assertion(value) match {
      case Left(error) => Left(error.render(value.toString))
      case Right(_)    => Right(unsafeApply(value))
    }

  def unsafeApply(a: A): T = a.asInstanceOf[T]
}

object Refined {
  def apply[A, T](assertion: Assertion[A]): Refined[_, A, T] = macro Macros.makeRefined[A, T]
}
