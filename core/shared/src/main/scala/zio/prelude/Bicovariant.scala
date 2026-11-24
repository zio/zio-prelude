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

import zio.Exit
import zio.prelude.newtypes.Failure

/**
 * Abstracts over a type constructor with two parameters that is covariant in
 * its second (right) parameter. [[Bicovariant]] and [[Divariant]] extend it.
 */
trait RightCovariant[<=>[_, +_]] {

  /**
   * Returns the `Covariant` instance for `A <=> *` that maps the right
   * parameter with `rightMap`.
   */
  def deriveCovariant[A]: Covariant[({ type lambda[+B] = A <=> B })#lambda] =
    new Covariant[({ type lambda[+B] = A <=> B })#lambda] {
      def map[B, C](f: B => C): A <=> B => A <=> C = rightMap(f)
    }

  /**
   * Lifts a function from `B` to `C` to a function that maps the right
   * parameter: `A <=> B` to `A <=> C`.
   */
  def rightMap[A, B, C](f: B => C): (A <=> B) => (A <=> C)

  // laws for rightMap

  /**
   * Returns whether mapping `ab` with `g compose f` equals mapping it with `f`
   * and then with `g`, which the composition law requires.
   */
  def rightMapCompose[A, B, B2, B3](
    ab: A <=> B,
    f: B => B2,
    g: B2 => B3
  )(implicit eq: Equal[A <=> B3]): Boolean = {
    val lhs: A <=> B => A <=> B3 = rightMap(g compose f)
    val rhs: A <=> B => A <=> B3 = rightMap(g) compose rightMap(f)
    eq.equal(lhs(ab), rhs(ab))
  }

  /**
   * Returns whether mapping `ab` with the identity function returns a value
   * equal to `ab`, which the identity law requires.
   */
  def rightMapIdentity[A, B](
    ab: A <=> B
  )(implicit eq: Equal[A <=> B]): Boolean = {
    val lhs: A <=> B => A <=> B = rightMap(identity[B])
    eq.equal(lhs(ab), ab)
  }
}

/**
 * Abstracts over a type constructor with two parameters that is covariant in
 * both, such as `Tuple2` and `Either`: both sides can be mapped, separately or
 * together with `bimap`.
 *
 * {{{
 * val pair: (Int, Int) = Bicovariant.Tuple2Bicovariant.bimap((_: Int) + 1, (_: String).length)((1, "ab"))
 * // (2,2)
 * val left: Either[Int, String] = (Left(1): Either[Int, String]).leftMap(_ + 1)
 * // Left(2)
 * }}}
 */
trait Bicovariant[<=>[+_, +_]] extends RightCovariant[<=>] { self =>

  /**
   * Returns the `Covariant` instance for the left parameter, through the
   * `Failure` newtype: `Failure[E <=> A]` maps `E` with `leftMap`.
   */
  def deriveFailureCovariant[A]: Covariant[({ type lambda[+E] = Failure[E <=> A] })#lambda] =
    new Covariant[({ type lambda[+E] = Failure[E <=> A] })#lambda] {
      def map[E, E1](f: E => E1): Failure[E <=> A] => Failure[E1 <=> A] = { exit =>
        Failure.wrap(self.leftMap(f)(Failure.unwrap(exit)))
      }
    }

  /**
   * Lifts two functions to a function that maps both parameters: the left
   * one with `f` and the right one with `g`.
   */
  def bimap[A, B, AA, BB](f: A => AA, g: B => BB): (A <=> B) => (AA <=> BB)

  /**
   * Lifts a function to a function that maps the left parameter.
   */
  def leftMap[A, B, AA](f: A => AA): (A <=> B) => (AA <=> B) =
    bimap(f, identity[B])

  /**
   * Lifts a function to a function that maps the right parameter.
   */
  def rightMap[A, B, BB](g: B => BB): (A <=> B) => (A <=> BB) =
    bimap(identity[A], g)

  // laws for leftMap

  /**
   * Returns whether mapping the left side of `ab` with `g compose f` equals
   * mapping it with `f` and then with `g`, which the composition law
   * requires.
   */
  def leftMapCompose[A, B, A2, A3](
    ab: A <=> B,
    f: A => A2,
    g: A2 => A3
  )(implicit eq: Equal[A3 <=> B]): Boolean = {
    val lhs: A <=> B => A3 <=> B = leftMap(g compose f)
    val rhs: A <=> B => A3 <=> B = leftMap(g) compose leftMap(f)
    eq.equal(lhs(ab), rhs(ab))
  }

  /**
   * Returns whether mapping the left side of `ab` with the identity function
   * returns a value equal to `ab`, which the identity law requires.
   */
  def leftMapIdentity[A, B](
    ab: A <=> B
  )(implicit eq: Equal[A <=> B]): Boolean = {
    val lhs: A <=> B => A <=> B = leftMap(identity[A])
    eq.equal(lhs(ab), ab)
  }

  // laws for bimap

  /**
   * Returns whether `bimap` with composed functions equals two `bimap`s in a
   * row, which the composition law requires.
   */
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

  /**
   * Returns whether `bimap` with two identity functions returns a value equal
   * to `ab`, which the identity law requires.
   */
  def bimapIdentity[A, B, B2, B3](
    ab: A <=> B
  )(implicit eq: Equal[A <=> B]): Boolean = {
    val lhs: A <=> B => A <=> B = bimap(identity[A], identity[B])
    eq.equal(lhs(ab), ab)
  }

  // bimap must be coherent with leftMap and rightMap

  /**
   * Returns whether `bimap(f, g)` equals `rightMap(g)` followed by
   * `leftMap(f)`, which coherence requires.
   */
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

/**
 * Instances of `Bicovariant`.
 */
object Bicovariant {

  /**
   * The `Bicovariant` instance for `Tuple2`, which maps the first and second
   * elements.
   */
  implicit val Tuple2Bicovariant: Bicovariant[Tuple2] =
    new Bicovariant[Tuple2] {
      override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): ((A, B)) => (AA, BB) = { case (a, b) => (f(a), g(b)) }
    }

  /**
   * The `Bicovariant` instance for `Either`, which maps the `Left` and `Right`
   * values.
   */
  implicit val EitherBicovariant: Bicovariant[Either] =
    new Bicovariant[Either] {
      override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): Either[A, B] => Either[AA, BB] = {
        case Right(a) => Right(g(a))
        case Left(b)  => Left(f(b))
      }
    }

  /**
   * The `Bicovariant` instance for `Exit`, which maps the error type (the
   * left parameter) and the success type (the right parameter).
   */
  implicit val ExitBicovariant: Bicovariant[Exit] =
    new Bicovariant[Exit] {
      override def bimap[A, E, AA, EE](f: A => AA, g: E => EE): Exit[A, E] => Exit[AA, EE] =
        _.mapBothExit(f, g)
    }
}

/**
 * Syntax for `Bicovariant`, available with `import zio.prelude._`.
 */
trait BicovariantSyntax {

  /**
   * Provides infix syntax for mapping both sides of a bicovariant value.
   */
  implicit class BicovariantOps[<=>[+_, +_], A, B](f: => A <=> B) {

    /**
     * Maps the left side with `g` and the right side with `h`.
     */
    def bimap[C, D](g: A => C, h: B => D)(implicit bicovariant: Bicovariant[<=>]): C <=> D =
      bicovariant.bimap(g, h)(f)

    /**
     * Maps the left side with `ac`.
     */
    def leftMap[C](ac: A => C)(implicit bicovariant: Bicovariant[<=>]): C <=> B =
      bicovariant.leftMap(ac)(f)

    /**
     * Maps the right side with `bc`.
     */
    def rightMap[C](bc: B => C)(implicit bicovariant: Bicovariant[<=>]): A <=> C =
      bicovariant.rightMap(bc)(f)
  }
}
