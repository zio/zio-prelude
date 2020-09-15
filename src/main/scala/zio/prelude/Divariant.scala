package zio.prelude

trait Divariant[:=>[-_, +_]] extends RightCovariant[:=>] { self =>

  def deriveContravariant[B]: Contravariant[({ type lambda[-A] = A :=> B })#lambda] =
    new Contravariant[({ type lambda[-A] = A :=> B })#lambda] {
      def contramap[A, C](f: C => A): A :=> B => C :=> B = leftContramap(f)
    }

  def dimap[A, B, C, D](f: C => A, g: B => D): (A :=> B) => (C :=> D) =
    (ab: A :=> B) => rightMap(g)(leftContramap(f)(ab))

  def leftContramap[A, B, C](f: C => A): (A :=> B) => (C :=> B)
}

object Divariant {
  final case class Join[:=>[-_, +_], A](value: A :=> A)

  implicit val Function1Divariant: Divariant[({ type lambda[-A, +B] = A => B })#lambda] =
    new Divariant[({ type lambda[-A, +B] = A => B })#lambda] {
      override def leftContramap[A, B, C](c2a: C => A): (A => B) => C => B = { a2b => c =>
        c |> c2a |> a2b
      }
      override def rightMap[A, B, C](b2c: B => C): (A => B) => A => C = { a2b => a =>
        a |> a2b |> b2c
      }
    }
}

trait DivariantSyntax {

  implicit class DivariantOps[:=>[-_, +_], A, B](f: => A :=> B) {

    def dimap[C, D](g: C => A, h: B => D)(implicit divariant: Divariant[:=>]): C :=> D =
      divariant.dimap(g, h)(f)

    def leftMap[C](ca: C => A)(implicit divariant: Divariant[:=>]): C :=> B =
      divariant.leftContramap(ca)(f)

    def rightMap[C](bc: B => C)(implicit divariant: Divariant[:=>]): A :=> C =
      divariant.rightMap(bc)(f)
  }
}
