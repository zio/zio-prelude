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

/**
 * Abstracts over a type constructor with two parameters that is contravariant
 * in its first (input) parameter and covariant in its second (output)
 * parameter, such as `Function1`. Both sides can be mapped, separately or
 * together with `dimap`.
 *
 * {{{
 * val double: Int => Int = _ * 2
 *
 * val describe: String => String =
 *   Divariant.Function1Divariant.dimap((s: String) => s.length, (n: Int) => s"n=\$n")(double)
 *
 * val described: String = describe("abc")
 * // n=6
 * }}}
 */
trait Divariant[=>:[-_, +_]] extends RightCovariant[=>:] { self =>

  /**
   * Returns the `Contravariant` instance for `* =>: B` that maps the input
   * parameter with `leftContramap`.
   */
  def deriveContravariant[B]: Contravariant[({ type lambda[-A] = A =>: B })#lambda] =
    new Contravariant[({ type lambda[-A] = A =>: B })#lambda] {
      def contramap[A, C](f: C => A): A =>: B => C =>: B = leftContramap(f)
    }

  /**
   * Lifts two functions to a function that maps both parameters: the input
   * with `f`, which converts the new input `C` to the old input `A`, and the
   * output with `g`.
   */
  def dimap[A, B, C, D](f: C => A, g: B => D): (A =>: B) => (C =>: D) =
    (ab: A =>: B) => rightMap(g)(leftContramap(f)(ab))

  /**
   * Lifts a function from `C` to `A` to a function that maps the input
   * parameter: `A =>: B` to `C =>: B`.
   */
  def leftContramap[A, B, C](f: C => A): (A =>: B) => (C =>: B)

  // laws for leftContramap

  /**
   * Returns whether contramapping `ab` with `f compose g` equals
   * contramapping it with `f` and then with `g`, which the composition law
   * requires.
   */
  def leftContramapCompose[A, B, A2, A3](
    ab: A =>: B,
    f: A2 => A,
    g: A3 => A2
  )(implicit eq: Equal[A3 =>: B]): Boolean = {
    val lhs: A =>: B => A3 =>: B = leftContramap(f compose g)
    val rhs: A =>: B => A3 =>: B = leftContramap(g) compose leftContramap(f)
    eq.equal(lhs(ab), rhs(ab))
  }

  /**
   * Returns whether contramapping `ab` with the identity function returns a
   * value equal to `ab`, which the identity law requires.
   */
  def leftContramapidentity[A, B](
    ab: A =>: B
  )(implicit eq: Equal[A =>: B]): Boolean = {
    val lhs: A =>: B => A =>: B = leftContramap(identity[A])
    eq.equal(lhs(ab), ab)
  }

  // laws for dimap

  /**
   * Returns whether `dimap` with composed functions equals two `dimap`s in a
   * row, which the composition law requires.
   */
  def dimapCompose[A, B, A2, A3, B2, B3](
    ab: A =>: B,
    g: A3 => A2,
    f: A2 => A,
    i: B => B2,
    h: B2 => B3
  )(implicit eq: Equal[A3 =>: B3]): Boolean = {
    val fg: A3 => A               = f compose g
    val hi: B => B3               = h compose i
    val lhs: A =>: B => A3 =>: B3 = dimap(fg, hi)

    val rhs1: A =>: B => A2 =>: B2   = dimap(f, i)
    val rhs2: A2 =>: B2 => A3 =>: B3 = dimap(g, h)
    val rhs3: A =>: B => A3 =>: B3   = rhs2 compose rhs1
    eq.equal(lhs(ab), rhs3(ab))
  }

  /**
   * Returns whether `dimap` with two identity functions returns a value equal
   * to `ab`, which the identity law requires.
   */
  def dimapIdentity[A, B, B2, B3](
    ab: A =>: B
  )(implicit eq: Equal[A =>: B]): Boolean = {
    val lhs: A =>: B => A =>: B = dimap(identity[A], identity[B])
    eq.equal(lhs(ab), ab)
  }

  // dimap must be coherent with leftContramap and rightMap

  /**
   * Returns whether `dimap(f, g)` equals `rightMap(g)` followed by
   * `leftContramap(f)`, which coherence requires.
   */
  def dimapCoherence[A, A2, A3, B, B2, B3](
    ab: A =>: B,
    f: A2 => A,
    g: B => B2
  )(implicit eq: Equal[A2 =>: B2]): Boolean = {
    val lhs: A =>: B => A2 =>: B2 = dimap(f, g)

    val rhs1: A =>: B => A =>: B2   = rightMap(g)
    val rhs2: A =>: B2 => A2 =>: B2 = leftContramap(f)
    val rhs3: A =>: B => A2 =>: B2  = rhs1 andThen rhs2

    eq.equal(lhs(ab), rhs3(ab))
  }
}

/**
 * Instances of `Divariant`.
 */
object Divariant {

  /**
   * Wraps a value of type `A =>: A`, whose input and output types are the
   * same.
   */
  final case class Join[=>:[-_, +_], A](value: A =>: A)

  /**
   * The `Divariant` instance for `Function1`: `leftContramap` runs a function
   * on the input first, and `rightMap` runs a function on the output.
   */
  implicit val Function1Divariant: Divariant[Function1] =
    new Divariant[Function1] {
      override def leftContramap[A, B, C](c2a: C => A): (A => B) => C => B = { a2b => c =>
        a2b(c2a(c))
      }
      override def rightMap[A, B, C](b2c: B => C): (A => B) => A => C      = { a2b => a =>
        b2c(a2b(a))
      }
    }
}

/**
 * Syntax for `Divariant`, available with `import zio.prelude._`.
 */
trait DivariantSyntax {

  /**
   * Provides infix syntax for mapping the input and output of a divariant
   * value.
   */
  implicit class DivariantOps[=>:[-_, +_], A, B](f: => A =>: B) {

    /**
     * Maps the input with `g`, which converts the new input `C` to `A`, and
     * the output with `h`.
     */
    def dimap[C, D](g: C => A, h: B => D)(implicit divariant: Divariant[=>:]): C =>: D =
      divariant.dimap(g, h)(f)

    /**
     * Maps the input with `ca`, which converts the new input `C` to `A`. It
     * calls `leftContramap`.
     */
    def leftMap[C](ca: C => A)(implicit divariant: Divariant[=>:]): C =>: B =
      divariant.leftContramap(ca)(f)

    /**
     * Maps the output with `bc`.
     */
    def rightMap[C](bc: B => C)(implicit divariant: Divariant[=>:]): A =>: C =
      divariant.rightMap(bc)(f)
  }
}
