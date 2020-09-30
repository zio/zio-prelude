package zio.prelude

import zio.Exit
import zio.prelude.newtypes.Failure

trait RightCovariant[<=>[_, +_]] {
  def deriveCovariant[A]: Covariant[({ type lambda[+B] = A <=> B })#lambda] =
    new Covariant[({ type lambda[+B] = A <=> B })#lambda] {
      def map[B, C](f: B => C): A <=> B => A <=> C = rightMap(f)
    }

  def rightMap[A, B, C](f: B => C): (A <=> B) => (A <=> C)

  // laws for leftMap

  def rightMapCompose[A, B, B2, B3](
    ab: A <=> B,
    f: B => B2,
    g: B2 => B3
  )(implicit eq: Equal[A <=> B3]): Boolean = {
    val lhs: A <=> B => A <=> B3 = rightMap(g compose f)
    val rhs: A <=> B => A <=> B3 = rightMap(g) compose rightMap(f)
    eq.equal(lhs(ab), rhs(ab))
  }

  def rightMapIdentity[A, B](
    ab: A <=> B
  )(implicit eq: Equal[A <=> B]): Boolean = {
    val lhs: A <=> B => A <=> B = rightMap(identity[B])
    eq.equal(lhs(ab), ab)
  }
}

trait Bicovariant[<=>[+_, +_]] extends RightCovariant[<=>] { self =>

  def deriveFailureCovariant[A]: Covariant[({ type lambda[+E] = Failure[E <=> A] })#lambda] =
    new Covariant[({ type lambda[+E] = Failure[E <=> A] })#lambda] {
      def map[E, E1](f: E => E1): Failure[E <=> A] => Failure[E1 <=> A] = { exit =>
        Failure.wrap(self.leftMap(f)(Failure.unwrap(exit)))
      }
    }

  def bimap[A, B, AA, BB](f: A => AA, g: B => BB): (A <=> B) => (AA <=> BB)

  def leftMap[A, B, AA](f: A => AA): (A <=> B) => (AA <=> B)  =
    bimap(f, identity[B])
  def rightMap[A, B, BB](g: B => BB): (A <=> B) => (A <=> BB) =
    bimap(identity[A], g)

  // laws for leftMap

  def leftMapCompose[A, B, A2, A3](
    ab: A <=> B,
    f: A => A2,
    g: A2 => A3
  )(implicit eq: Equal[A3 <=> B]): Boolean = {
    val lhs: A <=> B => A3 <=> B = leftMap(g compose f)
    val rhs: A <=> B => A3 <=> B = leftMap(g) compose leftMap(f)
    eq.equal(lhs(ab), rhs(ab))
  }

  def leftMapIdentity[A, B](
    ab: A <=> B
  )(implicit eq: Equal[A <=> B]): Boolean = {
    val lhs: A <=> B => A <=> B = leftMap(identity[A])
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

    val rhs1: A <=> B => A <=> B2   = rightMap(g)
    val rhs2: A <=> B2 => A2 <=> B2 = leftMap(f)
    val rhs3: A <=> B => A2 <=> B2  = rhs1 andThen rhs2

    eq.equal(lhs(ab), rhs3(ab))
  }
}

object Bicovariant {

  implicit val Tuple2Bicovariant: Bicovariant[Tuple2] =
    new Bicovariant[Tuple2] {
      override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): ((A, B)) => (AA, BB) = { case (a, b) => (f(a), g(b)) }
    }

  implicit val EitherBicovariant: Bicovariant[Either] =
    new Bicovariant[Either] {
      override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): Either[A, B] => Either[AA, BB] = {
        case Right(a) => Right(g(a))
        case Left(b)  => Left(f(b))
      }
    }

  implicit val ExitBicovariant: Bicovariant[Exit] =
    new Bicovariant[Exit] {
      override def bimap[A, E, AA, EE](f: A => AA, g: E => EE): Exit[A, E] => Exit[AA, EE] =
        _.bimap(f, g)
    }
}

trait BicovariantSyntax {

  implicit class BicovariantOps[<=>[+_, +_], A, B](f: => A <=> B) {

    def bimap[C, D](g: A => C, h: B => D)(implicit bicovariant: Bicovariant[<=>]): C <=> D =
      bicovariant.bimap(g, h)(f)

    def leftMap[C](ac: A => C)(implicit bicovariant: Bicovariant[<=>]): C <=> B =
      bicovariant.leftMap(ac)(f)

    def rightMap[C](bc: B => C)(implicit bicovariant: Bicovariant[<=>]): A <=> C =
      bicovariant.rightMap(bc)(f)
  }
}
