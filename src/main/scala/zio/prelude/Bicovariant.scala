package zio.prelude

import zio.Exit

trait RightCovariantLaws[<=>[_, +_]] extends RightCovariant[<=>] {

  // laws for map

  def rightMapCompose[A, B, B2, B3](
    ab: A <=> B,
    f: B => B2,
    g: B2 => B3
  )(implicit eq: Equal[A <=> B3]): Boolean = {
    val lhs: A <=> B => A <=> B3 = map(g compose f)
    val rhs: A <=> B => A <=> B3 = map(g) compose map(f)
    eq.equal(lhs(ab), rhs(ab))
  }

  def rightMapIdentity[A, B](
    ab: A <=> B
  )(implicit eq: Equal[A <=> B]): Boolean = {
    val lhs: A <=> B => A <=> B = map(identity[B])
    eq.equal(lhs(ab), ab)
  }
}

trait BicovariantLaws[<=>[+_, +_]] extends Bicovariant[<=>] with RightCovariantLaws[<=>] { self =>

  // laws for mapLeft

  def leftMapCompose[A, B, A2, A3](
    ab: A <=> B,
    f: A => A2,
    g: A2 => A3
  )(implicit eq: Equal[A3 <=> B]): Boolean = {
    val lhs: A <=> B => A3 <=> B = mapLeft(g compose f)
    val rhs: A <=> B => A3 <=> B = mapLeft(g) compose mapLeft(f)
    eq.equal(lhs(ab), rhs(ab))
  }

  def leftMapIdentity[A, B](
    ab: A <=> B
  )(implicit eq: Equal[A <=> B]): Boolean = {
    val lhs: A <=> B => A <=> B = mapLeft(identity[A])
    eq.equal(lhs(ab), ab)
  }

  // laws for bimap

  def bimapCompose[A, B, A2, A3, B2, B3](
    ab: A <=> B,
    g: A2 => A3,
    f: A => A2,
    i: B => B2,
    h: B2 => B3
  )(implicit eq: Equal[A3 <=> B3]): Boolean = {
    val fg: A => A3               = g compose f
    val hi: B => B3               = h compose i
    val lhs: A <=> B => A3 <=> B3 = bimap(fg, hi)

    val rhs1: A <=> B => A2 <=> B2   = bimap(f, i)
    val rhs2: A2 <=> B2 => A3 <=> B3 = bimap(g, h)
    val rhs3: A <=> B => A3 <=> B3   = rhs2 compose rhs1
    eq.equal(lhs(ab), rhs3(ab))
  }

  def bimapIdentity[A, B, B2, B3](
    ab: A <=> B
  )(implicit eq: Equal[A <=> B]): Boolean = {
    val lhs: A <=> B => A <=> B = bimap(identity[A], identity[B])
    eq.equal(lhs(ab), ab)
  }

  // bimap must be coherent with leftMap and rightMap

  def bimapCoherence[A, A2, A3, B, B2, B3](
    ab: A <=> B,
    f: A => A2,
    g: B => B2
  )(implicit eq: Equal[A2 <=> B2]): Boolean = {
    val lhs: A <=> B => A2 <=> B2 = bimap(f, g)

    val rhs1: A <=> B => A <=> B2   = map(g)
    val rhs2: A <=> B2 => A2 <=> B2 = mapLeft(f)
    val rhs3: A <=> B => A2 <=> B2  = rhs1 andThen rhs2

    eq.equal(lhs(ab), rhs3(ab))
  }
}

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
