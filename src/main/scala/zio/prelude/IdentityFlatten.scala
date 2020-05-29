package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.CovariantEqualFIdentityFlatten
import zio.test.TestResult
import zio.test.laws._

/**
 * `IdentityFlatten` described a type that can be "flattened" in an
 * associative way and has an identity element with respect to that operation.
 * For example, with a list we can always vacuosly add a layer by wrapping a
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

object IdentityFlatten extends LawfulF.Covariant[CovariantEqualFIdentityFlatten, Equal] {

  /**
   * Adding a layer by mapping a value and mapping it into the identity
   * element and then flattening is an identity.
   */
  val rightIdentityLaw = new ZLawsF.Covariant.Law1[CovariantEqualFIdentityFlatten, Equal]("rightIdentityLaw") {
    def apply[F[+_]: CovariantEqualFIdentityFlatten, A: Equal](fa: F[A]): TestResult =
      fa.map(a => IdentityFlatten[F].any.map(_ => a)).flatten <-> fa
  }

  /**
   * Adding a layer by mapping a value into the identity element and then
   * flattening is an identity
   */
  val leftIdentityLaw = new ZLawsF.Covariant.Law1[CovariantEqualFIdentityFlatten, Equal]("leftIdentityLaw") {
    def apply[F[+_]: CovariantEqualFIdentityFlatten, A: Equal](fa: F[A]): TestResult =
      IdentityFlatten[F].any.map(_ => fa).flatten <-> fa
  }

  /**
   * The set of all laws that instances of `IdentityFlatten` must satisfy.
   */
  val laws =
    rightIdentityLaw + leftIdentityLaw + AssociativeFlatten.laws

  /**
   * Summons an implicit `IdentityFlatten[F]`.
   */
  def apply[F[+_]](implicit identityFlatten: IdentityFlatten[F]): IdentityFlatten[F] =
    identityFlatten

  /**
   * The `IdentityFlatten` instance for `Option`.
   */
  implicit val IdentityFlattenOption: IdentityFlatten[Option] =
    new IdentityFlatten[Option] {
      def any: Option[Any]                              = Some(())
      def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa.flatten
    }
}
