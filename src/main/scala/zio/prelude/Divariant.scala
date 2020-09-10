package zio.prelude

trait Divariant[:=>[-_, +_]] { self =>
  def deriveCovariant[A]: Covariant[({ type lambda[+B] = A :=> B })#lambda] =
    new Covariant[({ type lambda[+B] = A :=> B })#lambda] {
      def map[B, C](f: B => C): A :=> B => A :=> C = dimap(identity, f)
    }

  def deriveContravariant[B]: Contravariant[({ type lambda[-A] = A :=> B })#lambda] =
    new Contravariant[({ type lambda[-A] = A :=> B })#lambda] {
      def contramap[A, C](f: C => A): A :=> B => C :=> B = dimap(f, identity)
    }

  def dimap[A, B, C, D](f: C => A, g: B => D): (A :=> B) => (C :=> D) =
    (ab: A :=> B) => rightMap(g)(leftMap(f)(ab))

  def leftMap[A, B, C](f: C => A): (A :=> B) => (C :=> B)

  def rightMap[A, B, C](f: B => C): (A :=> B) => (A :=> C)
}

object Divariant {
  final case class Join[:=>[-_, +_], A](value: A :=> A)

  implicit val Function1Divariant: Divariant[Function1[-*, +*]] = new Divariant[Function1[-*, +*]] {
    override def leftMap[A, B, C](c2a: C => A): (A => B) => C => B = { a2b => c =>
      c |> c2a |> a2b
    }
    override def rightMap[A, B, C](b2c: B => C): (A => B) => A => C = { a2b => a =>
      a |> a2b |> b2c
    }
  }
}
