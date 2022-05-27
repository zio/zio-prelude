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
import zio.prelude.coherent.CovariantDeriveEqual
import zio.test.TestResult
import zio.test.laws._

object CovariantLaws extends LawfulF.Covariant[CovariantDeriveEqual, Equal] {

  /**
   * Mapping with the identity function must be an identity function.
   */
  lazy val identityLaw: LawsF.Covariant[CovariantDeriveEqual, Equal] =
    new LawsF.Covariant.Law1[CovariantDeriveEqual, Equal]("identityLaw") {
      def apply[F[+_]: CovariantDeriveEqual, A: Equal](fa: F[A]): TestResult =
        fa.map(identity) <-> fa
    }

  /**
   * Mapping by `f` followed by `g` must be the same as mapping with the
   * composition of `f` and `g`.
   */
  lazy val compositionLaw: LawsF.Covariant[CovariantDeriveEqual, Equal] =
    new LawsF.Covariant.ComposeLaw[CovariantDeriveEqual, Equal]("compositionLaw") {
      def apply[F[+_]: CovariantDeriveEqual, A: Equal, B: Equal, C: Equal](fa: F[A], f: A => B, g: B => C): TestResult =
        fa.map(f).map(g) <-> fa.map(f andThen g)
    }

  /**
   * The set of all laws that instances of `Covariant` must satisfy.
   */
  lazy val laws: LawsF.Covariant[CovariantDeriveEqual, Equal] =
    identityLaw + compositionLaw
}
