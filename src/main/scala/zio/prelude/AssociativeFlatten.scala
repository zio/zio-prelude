package zio.prelude

import scala.annotation.implicitNotFound

import zio.prelude.coherent.AssociativeFlattenCovariantEqualF
import zio.test.TestResult
import zio.test.laws._

/**
 * `AssociativeFlatten` describes a type that can be "flattened" in an
 * associative way. For example, if we have a list of lists of lists, we can
 * flatten it by either flattening the two inner lists and then flattening the
 * resulting lists, or flattening the two outer lists and then flattening that
 * resulting list. Because the operation is associative, the resulting list is
 * the same either way.
 */
@implicitNotFound("No implicit AssociativeFlatten defined for ${F}.")
trait AssociativeFlatten[F[+_]] {

  /**
   * Flattens a value of type `F[F[A]]` to produce an `F[A]`.
   */
  def flatten[A](ffa: F[F[A]]): F[A]
}

object AssociativeFlatten extends LawfulF.Covariant[AssociativeFlattenCovariantEqualF, Equal] {

  /**
   * For all `fffa`, `flatten(flatten(fffa))` is equivalent to
   * `flatten(fffa.map(flatten))`.
   */
  val associativityLaw = new ZLawsF.Covariant.FlattenLaw[AssociativeFlattenCovariantEqualF, Equal]("associativityLaw") {
    def apply[F[+_]: AssociativeFlattenCovariantEqualF, A: Equal](fffa: F[F[F[A]]]): TestResult =
      fffa.flatten.flatten <-> fffa.map(_.flatten).flatten
  }

  /**
   * The set of all laws that instances of `AssociativeFlatten` must satisfy.
   */
  val laws = associativityLaw

  /**
   * Summons an implicit `AssociativeFlatten[F]`.
   */
  def apply[F[+_]](implicit associativeFlatten: AssociativeFlatten[F]): AssociativeFlatten[F] =
    associativeFlatten

  /**
   * The `AssociativeFlatten` instance for `Option`.
   */
  implicit val OptionAssociativeFlatten: AssociativeFlatten[Option] =
    new AssociativeFlatten[Option] {
      def flatten[A](ffa: Option[Option[A]]): Option[A] =
        ffa.flatten
    }
}

trait AssociativeFlattenSyntax {

  /**
   * Provides infix syntax for flattening covariant types..
   */
  implicit class AssociativeFlattenOps[F[+_], A](ffa: F[F[A]]) {

    /**
     * Flattens a value of type `F[F[A]]` to produce an `F[A]`.
     */
    def flatten(implicit flatten: AssociativeFlatten[F]): F[A] =
      flatten.flatten(ffa)
  }
}
