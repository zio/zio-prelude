/*
 * Copyright 2020-2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.prelude

import zio.prelude.newtypes.Failure
import zio.stm.ZSTM
import zio.stream.ZStream
import zio.{ZIO, ZLayer, ZManaged}

import scala.Predef.{identity => id}

/**
 * Abstract over type constructor with 3 parameters: on first as contravariant
 * and on second and third as covariant.
 */
trait Zivariant[Z[-_, +_, +_]] { self =>

  def deriveCovariant[R, E]: Covariant[({ type lambda[+A] = Z[R, E, A] })#lambda] =
    new Covariant[({ type lambda[+A] = Z[R, E, A] })#lambda] {
      def map[A, A1](f: A => A1): Z[R, E, A] => Z[R, E, A1] = self.map(f)
    }

  def deriveFailureCovariant[R, A]: Covariant[({ type lambda[+E] = Failure[Z[R, E, A]] })#lambda] =
    new Covariant[({ type lambda[+E] = Failure[Z[R, E, A]] })#lambda] {
      def map[E, E1](f: E => E1): Failure[Z[R, E, A]] => Failure[Z[R, E1, A]] =
        fa => Failure.wrap(self.mapLeft(f)(Failure.unwrap(fa)))
    }

  def deriveContravariant[E, A]: Contravariant[({ type lambda[-R] = Z[R, E, A] })#lambda] =
    new Contravariant[({ type lambda[-R] = Z[R, E, A] })#lambda] {
      def contramap[R, R1](f: R1 => R): Z[R, E, A] => Z[R1, E, A] = self.contramap(f)
    }

  def deriveDivariant[E]: Divariant[({ type lambda[-R, +A] = Z[R, E, A] })#lambda] =
    new Divariant[({ type lambda[-R, +A] = Z[R, E, A] })#lambda] {

      def leftContramap[R, A, RR](f: RR => R): Z[R, E, A] => Z[RR, E, A] = self.contramap(f)

      def rightMap[R, A, AA](f: A => AA): Z[R, E, A] => Z[R, E, AA] = self.map(f)
    }

  def zimap[R, E, A, R1, E1, A1](r: R1 => R, e: E => E1, a: A => A1): Z[R, E, A] => Z[R1, E1, A1]

  // derived methods
  def contramap[R, E, A, R1](r: R1 => R): Z[R, E, A] => Z[R1, E, A] =
    zimap(r, id[E], id[A])

  def mapLeft[R, E, A, E1](e: E => E1): Z[R, E, A] => Z[R, E1, A] =
    zimap(id[R], e, id[A])

  def map[R, E, A, A1](a: A => A1): Z[R, E, A] => Z[R, E, A1] =
    zimap(id[R], id[E], a)

  def bimap[R, E, A, E1, A1](e: E => E1, a: A => A1): Z[R, E, A] => Z[R, E1, A1] =
    zimap(id[R], e, a)

  def dimap[R, E, A, RR, AA](r: RR => R, a: A => AA): Z[R, E, A] => Z[RR, E, AA] =
    zimap(r, id[E], a)

  // zimap id id id == id
  def zimapIdentity[R, E, A](rea: Z[R, E, A])(implicit eq: Equal[Z[R, E, A]]): Boolean =
    EqualOps(zimap(id[R], id[E], id[A])(rea)) === rea // because of Dotty

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
    EqualOps(lhs) === rhs // because of Dotty
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
    EqualOps(lhs) === rhs2 // because of Dotty
  }
}

object Zivariant {

  implicit val FunctionEitherZivariant: Zivariant[({ type lambda[-R, +E, +A] = R => Either[E, A] })#lambda] =
    new Zivariant[({ type lambda[-R, +E, +A] = R => Either[E, A] })#lambda] {
      override def zimap[R, E, A, R1, E1, A1](
        r: R1 => R,
        e: E => E1,
        a: A => A1
      ): (R => Either[E, A]) => R1 => Either[E1, A1] =
        rea =>
          r1 =>
            (r andThen rea)(r1) match {
              case Right(aa) => Right(a(aa))
              case Left(ee)  => Left(e(ee))
            }
    }

  implicit val FunctionTupleZivariant: Zivariant[({ type lambda[-R, +E, +A] = R => (E, A) })#lambda] =
    new Zivariant[({ type lambda[-R, +E, +A] = R => (E, A) })#lambda] {
      override def zimap[R, E, A, R1, E1, A1](r: R1 => R, e: E => E1, a: A => A1): (R => (E, A)) => R1 => (E1, A1) =
        rea =>
          r1 => {
            val (ee, aa) = (r andThen rea)(r1)
            (e(ee), a(aa))
          }
    }

  implicit val ZioZivariant: Zivariant[ZIO] = new Zivariant[ZIO] {
    override def zimap[R, E, A, R1, E1, A1](r: R1 => R, e: E => E1, a: A => A1): ZIO[R, E, A] => ZIO[R1, E1, A1] =
      rea => rea.mapBoth(e, a).provideSome(r)
  }

  implicit val ZLayerZivariant: Zivariant[ZLayer] =
    new Zivariant[ZLayer] {
      override def zimap[E, A, R, EE, AA, RR](
        r: EE => E,
        e: A => AA,
        a: R => RR
      ): ZLayer[E, A, R] => ZLayer[EE, AA, RR] =
        rea => ZLayer.fromFunctionMany(r) >>> rea.map(a).mapError(e)
    }

  implicit val ZManagedZivariant: Zivariant[ZManaged] =
    new Zivariant[ZManaged] {
      override def zimap[E, A, R, EE, AA, RR](
        r: EE => E,
        e: A => AA,
        a: R => RR
      ): ZManaged[E, A, R] => ZManaged[EE, AA, RR] =
        rea => rea.mapBoth(e, a).provideSome(r)
    }

  implicit val ZStreamZivariant: Zivariant[ZStream] =
    new Zivariant[ZStream] {
      override def zimap[E, A, R, EE, AA, RR](
        r: EE => E,
        e: A => AA,
        a: R => RR
      ): ZStream[E, A, R] => ZStream[EE, AA, RR] =
        rea => rea.mapBoth(e, a).provideSome(r)
    }

  implicit val ZSTMZivariant: Zivariant[ZSTM] =
    new Zivariant[ZSTM] {
      override def zimap[E, A, R, EE, AA, RR](r: EE => E, e: A => AA, a: R => RR): ZSTM[E, A, R] => ZSTM[EE, AA, RR] =
        rea => rea.mapBoth(e, a).provideSome(r)
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
