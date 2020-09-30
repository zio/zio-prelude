package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.CovariantDeriveEqualIdentityFlatten
import zio.test.TestResult
import zio.test.laws._

/**
 * `IdentityFlatten` described a type that can be "flattened" in an
 * associative way and has an identity element with respect to that operation.
 * For example, with a list we can always vacuously add a layer by wrapping a
 * list in another list constructor and flattening the resulting list always
 * returns the original list unchanged.
 */
@implicitNotFound("No implicit IdentityFlatten defined for ${F}.")
trait IdentityFlatten[F[+_]] extends AssociativeFlatten[F] { self =>

  /**
   * The identity element.
   */
  def any: F[Any]
}

object IdentityFlatten extends LawfulF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] {

  /**
   * Adding a layer by mapping a value and mapping it into the identity
   * element and then flattening is an identity.
   */
  val rightIdentityLaw: LawsF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] =
    new LawsF.Covariant.Law1[CovariantDeriveEqualIdentityFlatten, Equal]("rightIdentityLaw") {
      def apply[F[+_]: CovariantDeriveEqualIdentityFlatten, A: Equal](fa: F[A]): TestResult =
        fa.map(a => IdentityFlatten[F].any.map(_ => a)).flatten <-> fa
    }

  /**
   * Adding a layer by mapping a value into the identity element and then
   * flattening is an identity
   */
  val leftIdentityLaw: LawsF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] =
    new LawsF.Covariant.Law1[CovariantDeriveEqualIdentityFlatten, Equal]("leftIdentityLaw") {
      def apply[F[+_]: CovariantDeriveEqualIdentityFlatten, A: Equal](fa: F[A]): TestResult =
        IdentityFlatten[F].any.map(_ => fa).flatten <-> fa
    }

  /**
   * The set of all laws that instances of `IdentityFlatten` must satisfy.
   */
  val laws: LawsF.Covariant[CovariantDeriveEqualIdentityFlatten, Equal] =
    rightIdentityLaw + leftIdentityLaw + AssociativeFlatten.laws

  /**
   * Summons an implicit `IdentityFlatten[F]`.
   */
  def apply[F[+_]](implicit identityFlatten: IdentityFlatten[F]): IdentityFlatten[F] =
    identityFlatten

}
