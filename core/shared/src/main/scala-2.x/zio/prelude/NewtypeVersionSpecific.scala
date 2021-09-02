package zio.prelude

import zio.NonEmptyChunk

trait NewtypeCompanionVersionSpecific {
  def refine[A](refinement: Refinement[A]): QuotedRefinement[A] = macro zio.prelude.Macros.refine_impl[A]

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype, ignoring any [[Refinement]].
   */
  def unsafeWrap[A, T <: NewtypeModule#Newtype[A]](newtype: T, value: A): T#Type = {
    val _ = newtype
    value.asInstanceOf[T#Type]
  }

  /**
   * Converts an instance of a type parameterized on the underlying type
   * to an instance of a type parameterized on the newtype, ignoring any
   * [[Refinement]]. For example, this could be used to convert a list of
   * instances of the underlying type to a list of instances of the newtype.
   */
  def unsafeWrapAll[F[_], A, T <: NewtypeModule#Newtype[A]](newtype: T, value: F[A]): F[T#Type] = {
    val _ = newtype
    value.asInstanceOf[F[T#Type]]
  }
}

trait NewtypeVersionSpecific[A] { self: NewtypeModule#Newtype[A] =>

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype.
   */
  def apply(value: A): Type = macro zio.prelude.Macros.wrap_impl[A, Type]

  def apply(value: A, values: A*): NonEmptyChunk[Type] = macro zio.prelude.Macros.applyMany_impl[A, Type]

  def make(value: A): Validation[String, Type] = macro zio.prelude.Macros.make_impl[A, Type]

  def refine(refinement: Refinement[A]): QuotedRefinement[A] = macro zio.prelude.Macros.refine_impl[A]

  /**
   * Converts an instance of a type parameterized on the underlying type
   * to an instance of a type parameterized on the newtype. For example,
   * this could be used to convert a list of instances of the underlying
   * type to a list of instances of the newtype.
   */
  def wrapAll[F[_]](value: F[A]): F[Type] = macro zio.prelude.Macros.wrapAll_impl[F, A, Type]

}
