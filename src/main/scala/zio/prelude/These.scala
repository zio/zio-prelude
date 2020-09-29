package zio.prelude

sealed trait These[+A, +B]
object These {
  final case class Left[+A](value: A)              extends These[A, Nothing]
  final case class Right[+B](value: B)             extends These[Nothing, B]
  final case class Both[+A, +B](left: A, right: B) extends These[A, B]

  /**
   * The `Bicovariant` instance for `These`.
   */
  val BicovariantThese: Bicovariant[These] = new Bicovariant[These] {

    override def bimap[R, E, A, E1, A1](f: E => E1, g: A => A1): These[E, A] => These[E1, A1] = {
      case Left(value)       => Left(f(value))
      case Right(value)      => Right(g(value))
      case Both(left, right) => Both(f(left), g(right))
    }
  }
}
