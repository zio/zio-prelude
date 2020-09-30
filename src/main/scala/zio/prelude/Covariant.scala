package zio.prelude

import zio.prelude.coherent.CovariantDeriveEqual
import zio.test.TestResult
import zio.test.laws._

trait CovariantSubset[F[+_], Subset[_]] {
  def mapSubset[A, B: Subset](f: A => B): F[A] => F[B]
}

/**
 * `Covariant[F]` provides implicit evidence that `F[+_]` is a covariant
 * endofunctor in the category of Scala objects.
 *
 * Covariant instances of type `F[A]` "produce" values of type `A` in some
 * sense. In some cases, such as with a `List[A]`, this means that they
 * contain values of type `A`, in which case we can simply access the elements
 * of the collection. In other cases it means that output values of type `A`
 * which may not already exists, such as with a `Function0[A]` that produces
 * `A` values when invoked.
 *
 * Common examples of covariant instances in ZIO includes effects with respect
 * to their error and value types, sinks with respect to their error and output
 * types, and queues and references with respect to their error and
 * output types.
 *
 * `Covariant` instances support a `map` operation which allows transforming
 * the output type given a function from the old output type to the new output
 * type. For example, if we have a `List[String]` and a function
 * `String => Int` that returns the length of a string, then we can construct
 * a `List[Int]` with the length of each string.
 */
trait Covariant[F[+_]] extends CovariantSubset[F, AnyType] with Invariant[F] {
  final def mapSubset[A, B: AnyType](f: A => B): F[A] => F[B] = map(f)

  /**
   * Lift a function from `A` to `B` to a function from `F[A]` to `F[B]`.
   */
  def map[A, B](f: A => B): F[A] => F[B]

  final def invmap[A, B](f: A <=> B): F[A] <=> F[B] =
    Equivalence((fa: F[A]) => map(f.to)(fa), (fb: F[B]) => map(f.from)(fb))
}

object Covariant extends LawfulF.Covariant[CovariantDeriveEqual, Equal] {

  /**
   * Mapping with the identity function must be an identity function.
   */
  val identityLaw: LawsF.Covariant[CovariantDeriveEqual, Equal] =
    new LawsF.Covariant.Law1[CovariantDeriveEqual, Equal]("identityLaw") {
      def apply[F[+_]: CovariantDeriveEqual, A: Equal](fa: F[A]): TestResult =
        fa.map(identity) <-> fa
    }

  /**
   * Mapping by `f` followed by `g` must be the same as mapping with the
   * composition of `f` and `g`.
   */
  val compositionLaw: LawsF.Covariant[CovariantDeriveEqual, Equal] =
    new LawsF.Covariant.ComposeLaw[CovariantDeriveEqual, Equal]("compositionLaw") {
      def apply[F[+_]: CovariantDeriveEqual, A: Equal, B: Equal, C: Equal](fa: F[A], f: A => B, g: B => C): TestResult =
        fa.map(f).map(g) <-> fa.map(f andThen g)
    }

  /**
   * The set of all laws that instances of `Covariant` must satisfy.
   */
  val laws: LawsF.Covariant[CovariantDeriveEqual, Equal] =
    identityLaw + compositionLaw

  /**
   * Summons an implicit `Covariant[F]`.
   */
  def apply[F[+_]](implicit covariant: Covariant[F]): Covariant[F] =
    covariant

}

trait CovariantSyntax {

  /**
   * Provides infix syntax for mapping over covariant values.
   */
  implicit class CovariantOps[F[+_], A](private val self: F[A]) {
    def as[B](b: => B)(implicit F: Covariant[F]): F[B] = map(_ => b)

    def map[B](f: A => B)(implicit F: Covariant[F]): F[B] =
      F.map(f)(self)

    def unit(implicit F: Covariant[F]): F[Unit] = as(())
  }
}
