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
    override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): These[A, B] => These[AA, BB] = {
      case Left(value)       => Left(f(value))
      case Right(value)      => Right(g(value))
      case Both(left, right) => Both(f(left), g(right))
    }
  }
}
