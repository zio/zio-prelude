package zio.prelude

import scala.Predef.{ identity => id }

import zio.prelude.Divariant.DivariantInstance
import zio.prelude.newtypes.Failure
import zio.stm.ZSTM
import zio.stream.ZStream
import zio.{ ZIO, ZLayer, ZManaged }

/**
 * Abstract over type constructor with 3 parameters: on first as contravariant
 * and on second and third as covariant.
 */
trait Zivariant[Z[-_, +_, +_]] { self =>

  def deriveCovariant[R, E]: Covariant[({ type lambda[+A] = Z[R, E, A] })#lambda] =
    new Covariant[({ type lambda[+A] = Z[R, E, A] })#lambda] {
      def map[A, A1](a: A => A1): Z[R, E, A] => Z[R, E, A1] = self.map(a)
    }

  def deriveFailureCovariant[R, A]: Covariant[({ type lambda[+E] = Failure[Z[R, E, A]] })#lambda] =
    new Covariant[({ type lambda[+E] = Failure[Z[R, E, A]] })#lambda] {
      type FZ[X] = Failure[Z[R, X, A]]
      def map[E, E1](e: E => E1): Failure[Z[R, E, A]] => Failure[Z[R, E1, A]] = fz => {
        val fz2: Z[R, E, A] => Z[R, E1, A] = self.mapLeft(e)
        Failure.wrap(fz2(Failure.unwrap(fz)))
      }
    }

  def deriveContravariant[E, A]: Contravariant[({ type lambda[-R] = Z[R, E, A] })#lambda] =
    new Contravariant[({ type lambda[-R] = Z[R, E, A] })#lambda] {
      def contramap[R, R1](r: R1 => R): Z[R, E, A] => Z[R1, E, A] = self.contramap(r)
    }

  def deriveDivariant[EE]: Divariant[({ type lambda[-R, +A] = Z[R, EE, A] })#lambda] =
    new DivariantInstance[({ type lambda[-R, +A] = Z[R, EE, A] })#lambda] {
      override def dimap[R, E, A, R1, A1](r: R1 => R, a: A => A1): Z[R, EE, A] => Z[R1, EE, A1] =
        self.dimap(r, a)
    }

  def contramap[R, E, A, R1](r: R1 => R): Z[R, E, A] => Z[R1, E, A]
  def mapLeft[R, E, A, E1](e: E => E1): Z[R, E, A] => Z[R, E1, A]
  def map[R, E, A, A1](a: A => A1): Z[R, E, A] => Z[R, E, A1]

  def bimap[R, E, A, E1, A1](e: E => E1, a: A => A1): Z[R, E, A] => Z[R, E1, A1]

  def dimap[R, E, A, R1, A1](r: R1 => R, a: A => A1): Z[R, E, A] => Z[R1, E, A1]

  def dimapLeft[R, E, A, R1, E1](r: R1 => R, e: E => E1): Z[R, E, A] => Z[R1, E1, A]

  def zimap[R, E, A, R1, E1, A1](r: R1 => R, e: E => E1, a: A => A1): Z[R, E, A] => Z[R1, E1, A1]

  // zimap id id id == id
  def zimapIdentity[R, E, A](rea: Z[R, E, A])(implicit eq: Equal[Z[R, E, A]]): Boolean =
    zimap(id[R], id[E], id[A])(rea) === rea

  // zimap (r2 andThen r1) (e1 andThen e2) (a1 andThen a2) == zimap (r1 e1 a1) andThen zimap (r2 e2 a2)
  def zimapComposition[R, E, A, R1, R2, E1, E2, A1, A2](
    rea: Z[R, E, A],
    r2: R2 => R1,
    r1: R1 => R,
    e1: E => E1,
    e2: E1 => E2,
    a1: A => A1,
    a2: A1 => A2
  )(implicit eq: Equal[Z[R2, E2, A2]]): Boolean = {
    val rhs: Z[R2, E2, A2] = zimap(r2 andThen r1, e1 andThen e2, a1 andThen a2)(rea)
    val lhs: Z[R2, E2, A2] = (zimap(r1, e1, a1) andThen zimap(r2, e2, a2))(rea)
    lhs === rhs
  }

  // zimap r e a == contramap(r) andThen map(a) andThen mapLeft(e)
  def zimapCoherentWithMapAndContramap[R, E, A, R1, E1, A1](
    rea: Z[R, E, A],
    r: R1 => R,
    e: E => E1,
    a: A => A1
  )(implicit eq: Equal[Z[R1, E1, A1]]): Boolean = {
    val lhs: Z[R1, E1, A1]                = zimap(r, e, a)(rea)
    val rhs1: Z[R, E, A] => Z[R1, E1, A1] = contramap(r) andThen map[R1, E, A, A1](a) andThen mapLeft(e)
    val rhs2: Z[R1, E1, A1]               = rhs1(rea)
    lhs === rhs2
  }
}

object Zivariant {

  trait ZimapZivariant[Z[-_, +_, +_]] extends Zivariant[Z] {

    def contramap[R, E, A, R1](r: R1 => R): Z[R, E, A] => Z[R1, E, A] = zimap(r, id[E], id[A])

    def mapLeft[R, E, A, E1](e: E => E1): Z[R, E, A] => Z[R, E1, A] = zimap(id[R], e, id[A])

    def map[R, E, A, A1](a: A => A1): Z[R, E, A] => Z[R, E, A1] = zimap(id[R], id[E], a)

    def bimap[R, E, A, E1, A1](e: E => E1, a: A => A1): Z[R, E, A] => Z[R, E1, A1] =
      zimap(id[R], e, a)

    def dimap[R, E, A, R1, A1](r: R1 => R, a: A => A1): Z[R, E, A] => Z[R1, E, A1] =
      zimap(r, id[E], a)

    def dimapLeft[R, E, A, R1, E1](r: R1 => R, e: E => E1): Z[R, E, A] => Z[R1, E1, A] =
      zimap(r, e, id[A])
  }

  def fromFunctionBicovariant[B[+_, +_]](implicit
    ev: Bicovariant[B]
  ): Zivariant[({ type lambda[-R, +E, +A] = R => B[E, A] })#lambda] =
    new ZimapZivariant[({ type lambda[-R, +E, +A] = R => B[E, A] })#lambda] {
      override def zimap[R, E, A, R1, E1, A1](
        r: R1 => R,
        e: E => E1,
        a: A => A1
      ): (R => B[E, A]) => R1 => B[E1, A1] =
        rea => r1 => (r andThen rea)(r1).bimap(e, a)
    }

  implicit val FunctionEitherZivariant: Zivariant[({ type lambda[-R, +E, +A] = R => Either[E, A] })#lambda] =
    fromFunctionBicovariant(Bicovariant.EitherBicovariant)

  implicit val FunctionTupleZivariant: Zivariant[({ type lambda[-R, +E, +A] = R => (E, A) })#lambda] =
    fromFunctionBicovariant(Bicovariant.Tuple2Bicovariant)

  implicit val ZioZivariant: Zivariant[ZIO] = new ZimapZivariant[ZIO] {
    override def zimap[R, E, A, R1, E1, A1](r: R1 => R, e: E => E1, a: A => A1): ZIO[R, E, A] => ZIO[R1, E1, A1] =
      rea => rea.bimap(e, a).provideSome(r)
  }

  implicit val ZLayerZivariant: Zivariant[ZLayer] =
    new ZimapZivariant[ZLayer] {
      override def zimap[E, A, R, EE, AA, RR](
        r: EE => E,
        e: A => AA,
        a: R => RR
      ): ZLayer[E, A, R] => ZLayer[EE, AA, RR] =
        rea => ZLayer.fromFunctionMany(r) >>> rea.map(a).mapError(e)
    }

  implicit val ZManagedZivariant: Zivariant[ZManaged] =
    new ZimapZivariant[ZManaged] {
      override def zimap[E, A, R, EE, AA, RR](
        r: EE => E,
        e: A => AA,
        a: R => RR
      ): ZManaged[E, A, R] => ZManaged[EE, AA, RR] =
        rea => rea.bimap(e, a).provideSome(r)
    }

  implicit val ZStreamZivariant: Zivariant[ZStream] =
    new ZimapZivariant[ZStream] {
      override def zimap[E, A, R, EE, AA, RR](
        r: EE => E,
        e: A => AA,
        a: R => RR
      ): ZStream[E, A, R] => ZStream[EE, AA, RR] =
        rea => rea.bimap(e, a).provideSome(r)
    }

  implicit val ZSTMZivariant: Zivariant[ZSTM] =
    new ZimapZivariant[ZSTM] {
      override def zimap[E, A, R, EE, AA, RR](r: EE => E, e: A => AA, a: R => RR): ZSTM[E, A, R] => ZSTM[EE, AA, RR] =
        rea => rea.bimap(e, a).provideSome(r)
    }
}

trait ZivariantSyntax {

  implicit class ZivariantOps[Z[-_, +_, +_], R, E, A](f: => Z[R, E, A]) {

    def zimap[R1, E1, A1](r: R1 => R, e: E => E1, a: A => A1)(implicit zivariant: Zivariant[Z]): Z[R1, E1, A1] =
      zivariant.zimap(r, e, a)(f)

    def contramap[R1](r: R1 => R)(implicit zivariant: Zivariant[Z]): Z[R1, E, A] =
      zivariant.contramap(r)(f)

    def mapLeft[E1](e: E => E1)(implicit zivariant: Zivariant[Z]): Z[R, E1, A] =
      zivariant.mapLeft(e)(f)

    def map[A1](a: A => A1)(implicit zivariant: Zivariant[Z]): Z[R, E, A1] =
      zivariant.map(a)(f)

    def bimap[E1, A1](e: E => E1, a: A => A1)(implicit zivariant: Zivariant[Z]): Z[R, E1, A1] =
      zivariant.bimap(e, a)(f)

    def dimap[RR, AA](r: RR => R, a: A => AA)(implicit zivariant: Zivariant[Z]): Z[RR, E, AA] =
      zivariant.dimap(r, a)(f)
  }
}
