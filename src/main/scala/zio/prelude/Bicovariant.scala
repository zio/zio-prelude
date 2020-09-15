package zio.prelude

import zio.stm.ZSTM
import zio.stream.ZStream
import zio.{ Exit, ZIO, ZLayer, ZManaged }

trait RightCovariant[:=>[_, +_]] {
  def deriveCovariant[A]: Covariant[({ type lambda[+B] = A :=> B })#lambda] =
    new Covariant[({ type lambda[+B] = A :=> B })#lambda] {
      def map[B, C](f: B => C): A :=> B => A :=> C = rightMap(f)
    }

  def rightMap[A, B, C](f: B => C): (A :=> B) => (A :=> C)
}

trait Bicovariant[:=>[+_, +_]] extends RightCovariant[:=>] {

  def bimap[A, B, AA, BB](f: A => AA, g: B => BB): (A :=> B) => (AA :=> BB)

  def leftMap[A, B, AA](f: A => AA): (A :=> B) => (AA :=> B)  =
    bimap(f, identity[B])
  def rightMap[A, B, BB](g: B => BB): (A :=> B) => (A :=> BB) =
    bimap(identity[A], g)
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

  implicit def ZIOBicovariant[R]: Bicovariant[({ type lambda[+E, +A] = ZIO[R, E, A] })#lambda] =
    new Bicovariant[({ type lambda[+E, +A] = ZIO[R, E, A] })#lambda] {
      override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): ZIO[R, A, B] => ZIO[R, AA, BB] =
        _.bimap(f, g)
    }

  implicit def ZstreamBicovariant[R]: Bicovariant[({ type lambda[+E, +O] = ZStream[R, E, O] })#lambda] =
    new Bicovariant[({ type lambda[+E, +O] = ZStream[R, E, O] })#lambda] {
      override def bimap[A, O, AA, OO](f: A => AA, g: O => OO): ZStream[R, A, O] => ZStream[R, AA, OO] =
        _.bimap(f, g)
    }

  implicit def ZManagedBicovariant[R]: Bicovariant[({ type lambda[+E, +O] = ZManaged[R, E, O] })#lambda] =
    new Bicovariant[({ type lambda[+E, +O] = ZManaged[R, E, O] })#lambda] {
      override def bimap[A, O, AA, OO](f: A => AA, g: O => OO): ZManaged[R, A, O] => ZManaged[R, AA, OO] =
        _.bimap(f, g)
    }

  implicit def ZSTMBicovariant[R]: Bicovariant[({ type lambda[+E, +A] = ZSTM[R, E, A] })#lambda] =
    new Bicovariant[({ type lambda[+E, +A] = ZSTM[R, E, A] })#lambda] {
      override def bimap[A, E, AA, EE](f: A => AA, g: E => EE): ZSTM[R, A, E] => ZSTM[R, AA, EE] =
        _.bimap(f, g)
    }

  implicit val ExitBicovariant: Bicovariant[Exit] =
    new Bicovariant[Exit] {
      override def bimap[A, E, AA, EE](f: A => AA, g: E => EE): Exit[A, E] => Exit[AA, EE] =
        _.bimap(f, g)
    }

  implicit def ZlayerBicovariant[RIn]: Bicovariant[({ type lambda[+E, +ROut] = ZLayer[RIn, E, ROut] })#lambda] =
    new Bicovariant[({ type lambda[+E, +ROut] = ZLayer[RIn, E, ROut] })#lambda] {
      override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): ZLayer[RIn, A, B] => ZLayer[RIn, AA, BB] =
        _.map(g).mapError(f)
    }
}

trait BicovariantSyntax {

  implicit class BicovariantOps[:=>[+_, +_], A, B](f: => A :=> B) {

    def bimap[C, D](g: A => C, h: B => D)(implicit bicovariant: Bicovariant[:=>]): C :=> D =
      bicovariant.bimap(g, h)(f)

    def leftMap[C](ac: A => C)(implicit bicovariant: Bicovariant[:=>]): C :=> B =
      bicovariant.leftMap(ac)(f)

    def rightMap[C](bc: B => C)(implicit bicovariant: Bicovariant[:=>]): A :=> C =
      bicovariant.rightMap(bc)(f)
  }
}
