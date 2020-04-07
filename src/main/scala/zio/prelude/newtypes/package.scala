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
   * A newtype represeting logical conjunction.
   */
  type And = And.Type

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
}
