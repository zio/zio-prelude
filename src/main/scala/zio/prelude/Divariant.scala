package zio.prelude

trait Divariant[:=>[-_, +_]] extends RightCovariant[:=>] { self =>

  def deriveContravariant[B]: Contravariant[({ type lambda[-A] = A :=> B })#lambda] =
    new Contravariant[({ type lambda[-A] = A :=> B })#lambda] {
      def contramap[A, C](f: C => A): A :=> B => C :=> B = leftContramap(f)
    }

  def dimap[A, B, C, D](f: C => A, g: B => D): (A :=> B) => (C :=> D) =
    (ab: A :=> B) => rightMap(g)(leftContramap(f)(ab))

  def leftContramap[A, B, C](f: C => A): (A :=> B) => (C :=> B)

  // laws for leftContramap

  def leftContramapCompose[A, B, A2, A3](
    ab: A :=> B,
    f: A2 => A,
    g: A3 => A2
  )(implicit eq: Equal[A3 :=> B]): Boolean = {
    val lhs: A :=> B => A3 :=> B = leftContramap(f compose g)
    val rhs: A :=> B => A3 :=> B = leftContramap(g) compose leftContramap(f)
    eq.equal(lhs(ab), rhs(ab))
  }

  def leftContramapidentity[A, B](
    ab: A :=> B
  )(implicit eq: Equal[A :=> B]): Boolean = {
    val lhs: A :=> B => A :=> B = leftContramap(identity[A])
    eq.equal(lhs(ab), ab)
  }

  // laws for dimap

  def dimapCompose[A, B, A2, A3, B2, B3](
    ab: A :=> B,
    g: A3 => A2,
    f: A2 => A,
    i: B => B2,
    h: B2 => B3
  )(implicit eq: Equal[A3 :=> B3]): Boolean = {
    val fg: A3 => A               = f compose g
    val hi: B => B3               = h compose i
    val lhs: A :=> B => A3 :=> B3 = dimap(fg, hi)

    val rhs1: A :=> B => A2 :=> B2   = dimap(f, i)
    val rhs2: A2 :=> B2 => A3 :=> B3 = dimap(g, h)
    val rhs3: A :=> B => A3 :=> B3   = rhs2 compose rhs1
    eq.equal(lhs(ab), rhs3(ab))
  }

  def dimapIdentity[A, B, B2, B3](
    ab: A :=> B
  )(implicit eq: Equal[A :=> B]): Boolean = {
    val lhs: A :=> B => A :=> B = dimap(identity[A], identity[B])
    eq.equal(lhs(ab), ab)
  }

  // dimap must be coherent with leftContramap and rightMap

  def dimapCoherence[A, A2, A3, B, B2, B3](
    ab: A :=> B,
    f: A2 => A,
    g: B => B2
  )(implicit eq: Equal[A2 :=> B2]): Boolean = {
    val lhs: A :=> B => A2 :=> B2 = dimap(f, g)

    val rhs1: A :=> B => A :=> B2   = rightMap(g)
    val rhs2: A :=> B2 => A2 :=> B2 = leftContramap(f)
    val rhs3: A :=> B => A2 :=> B2  = rhs1 andThen rhs2

    eq.equal(lhs(ab), rhs3(ab))
  }
}

object Divariant {
  final case class Join[:=>[-_, +_], A](value: A :=> A)

  implicit val Function1Divariant: Divariant[Function1] =
    new Divariant[Function1] {
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
