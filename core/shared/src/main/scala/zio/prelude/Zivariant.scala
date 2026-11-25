/*
 * Copyright 2020-2023 John A. De Goes and the ZIO Contributors
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
import zio.{ZEnvironment, ZIO}

import scala.Predef.{identity => id}

/**
 * Abstract over type constructor with 3 parameters: on first as contravariant
 * and on second and third as covariant.
 *
 * The three parameters have the shape of `ZIO[R, E, A]`: an environment, an
 * error and a value. Instances exist for `ZIO`, `ZStream`, `ZSTM`, and
 * functions from a `ZEnvironment` to an `Either` or a tuple. The environment
 * is mapped with a function between `ZEnvironment` values.
 */
trait Zivariant[Z[-_, +_, +_]] { self =>

  /**
   * Returns the `Covariant` instance for the value parameter, which maps it
   * with `map`.
   */
  def deriveCovariant[R, E]: Covariant[({ type lambda[+A] = Z[R, E, A] })#lambda] =
    new Covariant[({ type lambda[+A] = Z[R, E, A] })#lambda] {
      def map[A, A1](f: A => A1): Z[R, E, A] => Z[R, E, A1] = self.map(f)
    }

  /**
   * Returns the `Covariant` instance for the error parameter, through the
   * `Failure` newtype, which maps it with `mapLeft`.
   */
  def deriveFailureCovariant[R, A]: Covariant[({ type lambda[+E] = Failure[Z[R, E, A]] })#lambda] =
    new Covariant[({ type lambda[+E] = Failure[Z[R, E, A]] })#lambda] {
      def map[E, E1](f: E => E1): Failure[Z[R, E, A]] => Failure[Z[R, E1, A]] =
        fa => Failure.wrap(self.mapLeft(f)(Failure.unwrap(fa)))
    }

  /**
   * Lifts three functions to a function that maps all three parameters: the
   * environment with `r`, which converts the new environment to the old one,
   * the error with `e` and the value with `a`.
   */
  def zimap[R, E, A, R1, E1, A1](
    r: ZEnvironment[R1] => ZEnvironment[R],
    e: E => E1,
    a: A => A1
  ): Z[R, E, A] => Z[R1, E1, A1]

  // derived methods

  /**
   * Maps the environment with `r`, which converts the new environment to the
   * old one.
   */
  def contramap[R, E, A, R1](r: ZEnvironment[R1] => ZEnvironment[R]): Z[R, E, A] => Z[R1, E, A] =
    zimap(r, id[E], id[A])

  /**
   * Maps the error with `e`.
   */
  def mapLeft[R, E, A, E1](e: E => E1): Z[R, E, A] => Z[R, E1, A] =
    zimap(id[ZEnvironment[R]], e, id[A])

  /**
   * Maps the value with `a`.
   */
  def map[R, E, A, A1](a: A => A1): Z[R, E, A] => Z[R, E, A1] =
    zimap(id[ZEnvironment[R]], id[E], a)

  /**
   * Maps the error with `e` and the value with `a`.
   */
  def bimap[R, E, A, E1, A1](e: E => E1, a: A => A1): Z[R, E, A] => Z[R, E1, A1] =
    zimap(id[ZEnvironment[R]], e, a)

  /**
   * Maps the environment with `r`, which converts the new environment to the
   * old one, and the value with `a`.
   */
  def dimap[R, E, A, RR, AA](r: ZEnvironment[RR] => ZEnvironment[R], a: A => AA): Z[R, E, A] => Z[RR, E, AA] =
    zimap(r, id[E], a)

  // zimap id id id == id
  /**
   * Returns whether `zimap` with three identity functions returns a value
   * equal to `rea`, which the identity law requires.
   */
  def zimapIdentity[R, E, A](rea: Z[R, E, A])(implicit eq: Equal[Z[R, E, A]]): Boolean =
    EqualOps(zimap(id[ZEnvironment[R]], id[E], id[A])(rea)) === rea // because of Dotty

  // zimap (r2 andThen r1) (e1 andThen e2) (a1 andThen a2) == zimap (r1 e1 a1) andThen zimap (r2 e2 a2)
  /**
   * Returns whether `zimap` with composed functions equals two `zimap`s in a
   * row, which the composition law requires.
   */
  def zimapComposition[R, E, A, R1, R2, E1, E2, A1, A2](
    rea: Z[R, E, A],
    r2: ZEnvironment[R2] => ZEnvironment[R1],
    r1: ZEnvironment[R1] => ZEnvironment[R],
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
  /**
   * Returns whether `zimap(r, e, a)` equals `contramap(r)`, then `map(a)`,
   * then `mapLeft(e)`, which coherence requires.
   */
  def zimapCoherentWithMapAndContramap[R, E, A, R1, E1, A1](
    rea: Z[R, E, A],
    r: ZEnvironment[R1] => ZEnvironment[R],
    e: E => E1,
    a: A => A1
  )(implicit eq: Equal[Z[R1, E1, A1]]): Boolean = {
    val lhs: Z[R1, E1, A1]                = zimap(r, e, a)(rea)
    val rhs1: Z[R, E, A] => Z[R1, E1, A1] = contramap(r) andThen map[R1, E, A, A1](a) andThen mapLeft(e)
    val rhs2: Z[R1, E1, A1]               = rhs1(rea)
    EqualOps(lhs) === rhs2 // because of Dotty
  }
}

/**
 * Instances of `Zivariant`.
 */
object Zivariant {

  /**
   * The `Zivariant` instance for functions from a `ZEnvironment[R]` to an
   * `Either[E, A]`.
   */
  implicit val FunctionEitherZivariant
    : Zivariant[({ type lambda[-R, +E, +A] = ZEnvironment[R] => Either[E, A] })#lambda] =
    new Zivariant[({ type lambda[-R, +E, +A] = ZEnvironment[R] => Either[E, A] })#lambda] {
      override def zimap[R, E, A, R1, E1, A1](
        r: ZEnvironment[R1] => ZEnvironment[R],
        e: E => E1,
        a: A => A1
      ): (ZEnvironment[R] => Either[E, A]) => ZEnvironment[R1] => Either[E1, A1] =
        rea =>
          r1 =>
            (r andThen rea)(r1) match {
              case Right(aa) => Right(a(aa))
              case Left(ee)  => Left(e(ee))
            }
    }

  /**
   * The `Zivariant` instance for functions from a `ZEnvironment[R]` to a
   * tuple `(E, A)`.
   */
  implicit val FunctionTupleZivariant: Zivariant[({ type lambda[-R, +E, +A] = ZEnvironment[R] => (E, A) })#lambda] =
    new Zivariant[({ type lambda[-R, +E, +A] = ZEnvironment[R] => (E, A) })#lambda] {
      override def zimap[R, E, A, R1, E1, A1](
        r: ZEnvironment[R1] => ZEnvironment[R],
        e: E => E1,
        a: A => A1
      ): (ZEnvironment[R] => (E, A)) => ZEnvironment[R1] => (E1, A1) =
        rea =>
          r1 => {
            val (ee, aa) = (r andThen rea)(r1)
            (e(ee), a(aa))
          }
    }

  /**
   * The `Zivariant` instance for `ZIO`. It maps the error and value with
   * `mapBoth` and the environment with `provideSomeEnvironment`.
   */
  implicit val ZioZivariant: Zivariant[ZIO] = new Zivariant[ZIO] {
    override def zimap[R, E, A, R1, E1, A1](
      r: ZEnvironment[R1] => ZEnvironment[R],
      e: E => E1,
      a: A => A1
    ): ZIO[R, E, A] => ZIO[R1, E1, A1] =
      rea => rea.mapBoth(e, a).provideSomeEnvironment(r)
  }

  /**
   * The `Zivariant` instance for `ZStream`. It maps the error and the
   * elements with `mapBoth` and the environment with
   * `provideSomeEnvironment`.
   */
  implicit val ZStreamZivariant: Zivariant[ZStream] =
    new Zivariant[ZStream] {
      override def zimap[E, A, R, EE, AA, RR](
        r: ZEnvironment[EE] => ZEnvironment[E],
        e: A => AA,
        a: R => RR
      ): ZStream[E, A, R] => ZStream[EE, AA, RR] =
        rea => rea.mapBoth(e, a).provideSomeEnvironment(r)
    }

  /**
   * The `Zivariant` instance for `ZSTM`. It maps the error and value with
   * `mapBoth` and the environment with `provideSomeEnvironment`.
   */
  implicit val ZSTMZivariant: Zivariant[ZSTM] =
    new Zivariant[ZSTM] {
      override def zimap[E, A, R, EE, AA, RR](
        r: ZEnvironment[EE] => ZEnvironment[E],
        e: A => AA,
        a: R => RR
      ): ZSTM[E, A, R] => ZSTM[EE, AA, RR] =
        rea => rea.mapBoth(e, a).provideSomeEnvironment(r)
    }
}

/**
 * Syntax for `Zivariant`, available with `import zio.prelude._`.
 */
trait ZivariantSyntax {

  /**
   * Provides infix syntax for mapping the environment, error and value of a
   * zivariant value. A type's own method with the same name, such as
   * `ZIO#map`, takes precedence.
   */
  implicit class ZivariantOps[Z[-_, +_, +_], R, E, A](f: => Z[R, E, A]) {

    /**
     * Maps the environment with `r`, the error with `e` and the value with
     * `a`.
     */
    def zimap[R1, E1, A1](r: ZEnvironment[R1] => ZEnvironment[R], e: E => E1, a: A => A1)(implicit
      zivariant: Zivariant[Z]
    ): Z[R1, E1, A1] =
      zivariant.zimap(r, e, a)(f)

    /**
     * Maps the environment with `r`, which converts the new environment to
     * the old one.
     */
    def contramap[R1](r: ZEnvironment[R1] => ZEnvironment[R])(implicit zivariant: Zivariant[Z]): Z[R1, E, A] =
      zivariant.contramap(r)(f)

    /**
     * Maps the error with `e`.
     */
    def mapLeft[E1](e: E => E1)(implicit zivariant: Zivariant[Z]): Z[R, E1, A] =
      zivariant.mapLeft(e)(f)

    /**
     * Maps the value with `a`.
     */
    def map[A1](a: A => A1)(implicit zivariant: Zivariant[Z]): Z[R, E, A1] =
      zivariant.map(a)(f)

    /**
     * Maps the error with `e` and the value with `a`.
     */
    def bimap[E1, A1](e: E => E1, a: A => A1)(implicit zivariant: Zivariant[Z]): Z[R, E1, A1] =
      zivariant.bimap(e, a)(f)

    /**
     * Maps the environment with `r` and the value with `a`.
     */
    def dimap[RR, AA](r: ZEnvironment[RR] => ZEnvironment[R], a: A => AA)(implicit
      zivariant: Zivariant[Z]
    ): Z[RR, E, AA] =
      zivariant.dimap(r, a)(f)
  }
}
