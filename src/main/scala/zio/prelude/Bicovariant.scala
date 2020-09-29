package zio.prelude

import zio.Exit

object Bicovariant {

  implicit val Tuple2Bicovariant: Bicovariant[Tuple2] =
    new Bicovariant[Tuple2] {

      override def bimap[R, E, A, E1, A1](f: E => E1, g: A => A1): ((E, A)) => (E1, A1) = { case (a, b) =>
        (f(a), g(b))
      }
    }

  implicit val EitherBicovariant: Bicovariant[Either] =
    new Bicovariant[Either] {

      override def bimap[R, E, A, E1, A1](f: E => E1, g: A => A1): Either[E, A] => Either[E1, A1] = {
        case Right(a) => Right(g(a))
        case Left(b)  => Left(f(b))
      }
    }

  implicit val ExitBicovariant: Bicovariant[Exit] =
    new Bicovariant[Exit] {
      override def bimap[R, E, A, E1, A1](f: E => E1, g: A => A1): Exit[E, A] => Exit[E1, A1] =
        _.bimap(f, g)
    }

}

trait BicovariantSyntax {

  implicit class BicovariantOps[<=>[+_, +_], A, B](f: => A <=> B) {

    def bimap[C, D](g: A => C, h: B => D)(implicit bicovariant: Bicovariant[<=>]): C <=> D =
      bicovariant.bimap(g, h)(f)

    def leftMap[C](ac: A => C)(implicit bicovariant: Bicovariant[<=>]): C <=> B =
      bicovariant.mapLeft(ac)(f)

    def rightMap[C](bc: B => C)(implicit bicovariant: Bicovariant[<=>]): A <=> C =
      bicovariant.map(bc)(f)
  }
}
