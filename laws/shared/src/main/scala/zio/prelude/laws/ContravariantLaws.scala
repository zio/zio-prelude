/*
 * Copyright 2021-2022 John A. De Goes and the ZIO Contributors
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

package zio.prelude.laws

import zio.prelude._
import zio.prelude.coherent.ContravariantDeriveEqual
import zio.test.TestResult
import zio.test.laws._

object ContravariantLaws extends LawfulF.Contravariant[ContravariantDeriveEqual, Equal] {

  /**
   * Contramapping with the identity function must not change the structure.
   */
  lazy val identityLaw: LawsF.Contravariant[ContravariantDeriveEqual, Equal] =
    new LawsF.Contravariant.Law1[ContravariantDeriveEqual, Equal]("identityLaw") {
      def apply[F[-_]: ContravariantDeriveEqual, A: Equal](fa: F[A]): TestResult =
        fa.contramap(identity[A]) <-> fa
    }

  /**
   * Contramapping by `f` followed by `g` must be the same as contramapping
   * with the composition of `f` and `g`.
   */
  lazy val compositionLaw: LawsF.Contravariant[ContravariantDeriveEqual, Equal] =
    new LawsF.Contravariant.ComposeLaw[ContravariantDeriveEqual, Equal]("compositionLaw") {
      def apply[F[-_]: ContravariantDeriveEqual, A: Equal, B: Equal, C: Equal](
        fa: F[A],
        f: B => A,
        g: C => B
      ): TestResult = {
        // Dotty can't infer this https://github.com/zio/zio-prelude/issues/273
        implicit val equalFC: Equal[F[C]] = Equal.DeriveEqual[F, C]
        fa.contramap(f).contramap(g) <-> fa.contramap(f compose g)
      }
    }

  /**
   * The set of all laws that instances of `Contravariant` must satisfy.
   */
  lazy val laws: LawsF.Contravariant[ContravariantDeriveEqual, Equal] =
    identityLaw + compositionLaw
}
