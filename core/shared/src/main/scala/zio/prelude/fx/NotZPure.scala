package zio.prelude.fx

/**
 * Provides implicit evidence that a type is not a subtype of `ZPure`.
 */
trait NotZPure[A]

object NotZPure {

  implicit def notZPure[A]: NotZPure[A]                                                    = new NotZPure[A] {}
  implicit def notZPureAmbiguous1[W, S1, S2, R, E, A]: NotZPure[ZPure[W, S1, S2, R, E, A]] =
    new NotZPure[ZPure[W, S1, S2, R, E, A]] {}
  implicit def notZPureAmbiguous2[W, S1, S2, R, E, A]: NotZPure[ZPure[W, S1, S2, R, E, A]] =
    new NotZPure[ZPure[W, S1, S2, R, E, A]] {}
}
