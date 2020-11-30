package zio.prelude

package object newtypes {
  object Sum extends SubtypeF

  /**
   * A newtype representing addition.
   */
  type Sum[A] = Sum.Type[A]

  object Prod extends SubtypeF

  /**
   * A newtype representing multiplication.
   */
  type Prod[A] = Prod.Type[A]

  object Or extends Subtype[Boolean]

  /**
   * A newtype representing logical disjunction.
   */
  type Or = Or.Type

  object And extends Subtype[Boolean]

  /**
   * A newtype representing logical conjunction.
   */
  type And = And.Type

  object AndF extends SubtypeF

  /**
   * A newtype representing parameterized logical conjunction.
   */
  type AndF[+A] = AndF.Type[A]

  object OrF extends SubtypeF

  /**
   * A newtype representing parameterized logical disjunction.
   */
  type OrF[+A] = OrF.Type[A]

  object First extends SubtypeF

  /**
   * A newtype representing taking the first of two elements.
   */
  type First[A] = First.Type[A]

  object Last extends SubtypeF

  /**
   * A newtype representing taking the last of two elements.
   */
  type Last[A] = Last.Type[A]

  object Min extends SubtypeF

  /**
   * A newtype representing taking the min of two elements.
   */
  type Min[A] = Min.Type[A]

  object Max extends SubtypeF

  /**
   * A newtype representing taking the max of two elements.
   */
  type Max[A] = Max.Type[A]

  /**
   * A newtype representing another type in a failed state
   */
  object Failure extends NewtypeF

  type Failure[+A] = Failure.Type[A]

  /**
   * A newtype representing an input error in another type
   */
  object FailureIn extends NewtypeF

  type FailureIn[+A] = FailureIn.Type[A]

  /**
   * A newtype representing an output error in another type
   */
  object FailureOut extends NewtypeF

  type FailureOut[+A] = FailureOut.Type[A]

  /**
   * A newtype representing Right-to-left composition of functors.
   *
   * If F[_] and G[_] are both Covariant, then Nested[F, G, *] is also a Covariant.
   *
   * If F[_] and G[_] are both IdentityBoth, then Nested[F, G, *] is also an IdentityBoth.
   *
   * If F[_] and G[_] are both Traversable, then Nested[F, G, *] is also a Traversable.
   */
  object Nested extends NewtypeF

  type Nested[F[+_], G[+_], +A] = Nested.Type[F[G[A]]]

  /**
   * A newtype representing the product of functors.
   *
   * If F[_] and G[_] are both Covariant, then Product[F, G, *] is also a Covariant.
   *
   * If F[_] and G[_] are both IdentityBoth, then Product[F, G, *] is also an IdentityBoth.
   *
   * If F[_] and G[_] are both Traversable, then Product[F, G, *] is also a Traversable.
   */
  object Product extends NewtypeF

  type Product[F[+_], G[+_], +A] = Product.Type[(F[A], G[A])]
}
