package zio.prelude

object Divariant {

  final case class Join[:=>[-_, +_], A](value: A :=> A)

  implicit val Function1Divariant: Divariant[({ type lambda[-A, +B] = A => B })#lambda] =
    new Divariant[({ type lambda[-A, +B] = A => B })#lambda] {
      override def zimap[R, E, A, R1, E1, A1](r: R1 => R, e: E => E1, a: A => A1): (R => A) => R1 => A1 =
        r andThen _ andThen a
    }
}

trait DivariantSyntax {

  implicit class DivariantOps[:=>[-_, +_], A, B](f: => A :=> B) {

    def dimap[C, D](g: C => A, h: B => D)(implicit divariant: Divariant[:=>]): C :=> D =
      divariant.dimap(g, h)(f)

    def leftMap[C](ca: C => A)(implicit divariant: Divariant[:=>]): C :=> B =
      divariant.contramap(ca)(f)

    def rightMap[C](bc: B => C)(implicit divariant: Divariant[:=>]): A :=> C =
      divariant.map(bc)(f)
  }
}
