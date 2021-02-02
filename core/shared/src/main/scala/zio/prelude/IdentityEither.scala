/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
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

import zio.prelude.coherent.DeriveEqualIdentityEitherInvariant
import zio.test.TestResult
import zio.test.laws._

import scala.annotation.implicitNotFound

/**
 * A binary operator that combines two values of types `F[A]` and `F[B]` to
 * produce an `F[Either[A, B]]` with an identity value.
 */
@implicitNotFound("No implicit IdentityEither defined for ${F}.")
trait IdentityEither[F[_]] extends AssociativeEither[F] {

  /**
   * The identity for combining two values of types `F[A]` and `F[B]` to
   * produce an `F[Either[A, B]]`.
   */
  def none: F[Nothing]
}

object IdentityEither extends LawfulF.Invariant[DeriveEqualIdentityEitherInvariant, Equal] {

  /**
   * For all `fa`, `either(identity, fa)` is equivalent to `fa`.
   */
  val leftIdentityLaw: LawsF.Invariant[DeriveEqualIdentityEitherInvariant, Equal] =
    new LawsF.Invariant.Law1[DeriveEqualIdentityEitherInvariant, Equal]("leftIdentityLaw") {
      def apply[F[_]: DeriveEqualIdentityEitherInvariant, A: Equal](fa: F[A]): TestResult = {
        val left  = IdentityEither[F].either[Nothing, A](IdentityEither[F].none, fa)
        val right = fa
        val left2 = Invariant[F].invmap(Equivalence.eitherNothing[A] compose Equivalence.eitherFlip).to(left)
        left2 <-> right
      }
    }

  /**
   * For all `fa`, `either(fa, identity)` is equivalent to `fa`.
   */
  val rightIdentityLaw: LawsF.Invariant[DeriveEqualIdentityEitherInvariant, Equal] =
    new LawsF.Invariant.Law1[DeriveEqualIdentityEitherInvariant, Equal]("rightIdentityLaw") {
      def apply[F[_]: DeriveEqualIdentityEitherInvariant, A: Equal](fa: F[A]): TestResult = {
        val left  = IdentityEither[F].either[A, Nothing](fa, IdentityEither[F].none)
        val right = fa
        val left2 = Invariant[F].invmap(Equivalence.eitherNothing[A]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `IdentityEither` must satisfy.
   */
  val laws: LawsF.Invariant[DeriveEqualIdentityEitherInvariant, Equal] =
    leftIdentityLaw + rightIdentityLaw + AssociativeEither.laws

  /**
   * Summons an implicit `IdentityEither[F]`.
   */
  def apply[F[_]](implicit identityEither: IdentityEither[F]): IdentityEither[F] =
    identityEither
}

trait IdentityEitherSyntax {

  implicit class IdentityEitherAnyOps(a: Any) {

    /** Ignores its argument and returns a "failed" `F` */
    def fail[F[_]](implicit either: IdentityEither[F]): F[Nothing] =
      either.none
  }

}
