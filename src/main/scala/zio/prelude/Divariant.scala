package zio.prelude

object Divariant {

  final case class Join[:=>[-_, +_], A](value: A :=> A)

  implicit val Function1Divariant: Divariant[Function1] =
    new Divariant[Function1] {
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
