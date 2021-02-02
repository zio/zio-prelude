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

import com.github.ghik.silencer.silent
import zio.prelude.coherent.DeriveEqualIdentityBothInvariant
import zio.test.TestResult
import zio.test.laws._

import scala.annotation.implicitNotFound

/**
 * A binary operator that combines two values of types `F[A]` and `F[B]` to
 * produce an `F[(A, B)]` with an identity.
 */
@implicitNotFound("No implicit IdentityBoth defined for ${F}.")
trait IdentityBoth[F[_]] extends AssociativeBoth[F] {

  /**
   * The identity for combining two values of types `F[A]` and `F[B]` to
   * produce an `F[(A, B)]`.
   */
  def any: F[Any]
}

@silent("Unused import")
object IdentityBoth extends LawfulF.Invariant[DeriveEqualIdentityBothInvariant, Equal] {
  import zio._ // for zio.EitherCompat

  /**
   * For all `fa`, `both(identity, fa)` is equivalent to `fa`.
   */
  val leftIdentityLaw: LawsF.Invariant[DeriveEqualIdentityBothInvariant, Equal] =
    new LawsF.Invariant.Law1[DeriveEqualIdentityBothInvariant, Equal]("leftIdentityLaw") {
      def apply[F[_]: DeriveEqualIdentityBothInvariant, A: Equal](fa: F[A]): TestResult = {
        val left  = IdentityBoth[F].both(IdentityBoth[F].any, fa)
        val right = fa
        val left2 = Invariant[F].invmap(Equivalence.tupleAny[A] compose Equivalence.tupleFlip).to(left)
        left2 <-> right
      }
    }

  /**
   * For all `fa`, `both(fa, identity)` is equivalent to `fa`.
   */
  val rightIdentityLaw: LawsF.Invariant[DeriveEqualIdentityBothInvariant, Equal] =
    new LawsF.Invariant.Law1[DeriveEqualIdentityBothInvariant, Equal]("rightIdentityLaw") {
      def apply[F[_]: DeriveEqualIdentityBothInvariant, A: Equal](fa: F[A]): TestResult = {
        val left  = IdentityBoth[F].both(fa, IdentityBoth[F].any)
        val right = fa
        val left2 = Invariant[F].invmap(Equivalence.tupleAny[A]).to(left)
        left2 <-> right
      }
    }

  /**
   * The set of law laws that instances of `IdentityBoth` must satisfy.
   */
  val laws: LawsF.Invariant[DeriveEqualIdentityBothInvariant, Equal] =
    leftIdentityLaw + rightIdentityLaw + AssociativeBoth.laws

  /**
   * Summons an implicit `IdentityBoth[F]`.
   */
  def apply[F[_]](implicit identityBoth: IdentityBoth[F]): IdentityBoth[F] =
    identityBoth

  def fromCovariantIdentityFlatten[F[+_]](implicit
    covariant: Covariant[F],
    identityFlatten: IdentityFlatten[F]
  ): IdentityBoth[F] = new IdentityBoth[F] {

    override def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] = fa.map(a => fb.map(b => (a, b))).flatten

    override def any: F[Any] = identityFlatten.any

  }
}

trait IdentityBothSyntax {

  implicit class IdentityBothAnyOps[A](a: => A) {
    def succeed[F[+_]](implicit both: IdentityBoth[F], covariant: Covariant[F]): F[A] =
      both.any.map(_ => a)
  }

}
