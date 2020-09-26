package zio.prelude

import scala.Predef.{ identity => id }

object Divariant {

  trait DivariantInstance[Z[-_, +_]] extends Divariant[Z] {
    def contramap[R, E, A, R1](r: R1 => R): Z[R, A] => Z[R1, A] =
      dimap(r, id[A])

    def mapLeft[R, E, A, E1](e: E => E1): Z[R, A] => Z[R, A] =
      id

    def map[R, E, A, A1](a: A => A1): Z[R, A] => Z[R, A1] =
      dimap(id[R], a)

    def bimap[R, E, A, E1, A1](e: E => E1, a: A => A1): Z[R, A] => Z[R, A1] =
      map(a)

    def dimapLeft[R, E, A, R1, E1](r: R1 => R, e: E => E1): Z[R, A] => Z[R1, A] =
      contramap(r)

    def zimap[R, E, A, R1, E1, A1](r: R1 => R, e: E => E1, a: A => A1): Z[R, A] => Z[R1, A1] =
      dimap(r, a)
  }

  final case class Join[:=>[-_, +_], A](value: A :=> A)

  implicit val Function1Divariant: Divariant[Function1] =
    new DivariantInstance[Function1] {
      override def dimap[R, E, A, R1, A1](r: R1 => R, a: A => A1): (R => A) => R1 => A1 =
        r andThen _ andThen a
    }
}

trait DivariantSyntax {

  implicit class DivariantOps[:=>[-_, +_], A, B](f: => A :=> B) {

    def dimap[C, D](g: C => A, h: B => D)(implicit divariant: Divariant[:=>]): C :=> D =
      divariant.dimap(g, h)(f)

    def leftContramap[C](ca: C => A)(implicit divariant: Divariant[:=>]): C :=> B =
      divariant.contramap(ca)(f)

    def rightMap[C](bc: B => C)(implicit divariant: Divariant[:=>]): A :=> C =
      divariant.map(bc)(f)
  }
}
