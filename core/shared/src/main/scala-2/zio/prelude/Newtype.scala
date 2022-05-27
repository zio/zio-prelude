package zio.prelude

import zio.NonEmptyChunk

/**
 * The class of objects corresponding to newtypes. Users should implement an
 * object that extends this class to create their own newtypes, specifying
 * `A` as the underlying type to wrap.
 *
 * {{{
 * object Meter extends Newtype[Double]
 * type Meter = Meter.Type
 * }}}
 */

abstract class Newtype[A] {
  type Wrapped = A

  type Base
  trait Tag extends Any
  type Type = Base with Tag

  def assertion: QuotedAssertion[A] = new QuotedAssertion[A] {
    override def assertion: Assertion[A] = Assertion.anything
  }

  /**
   * Derives an instance of a type class for the new type given an instance
   * of the type class for the underlying type. The caller is responsible for
   * the type class being a valid instance for the new type.
   */
  protected def derive[TypeClass[_]](implicit instance: TypeClass[A]): TypeClass[Type] =
    instance.asInstanceOf[TypeClass[Type]]

  /**
   * Allows pattern matching on newtype instances to convert them back to
   * instances of the underlying type.
   */
  def unapply(value: Type): Some[A] = Some(unwrap(value))

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype. Ignores the assertion.
   */
  protected def wrap(value: A): Type = value.asInstanceOf[Type]

  /**
   * Converts an instance of the newtype back to an instance of the
   * underlying type.
   */
  def unwrap(value: Type): A = value.asInstanceOf[A]

  /**
   * Converts an instance of a type parameterized on the newtype back to an
   * instance of a type parameterized on the underlying type. For example,
   * this could be used to convert a list of instances of the newtype back
   * to a list of instances of the underlying type.
   */

  def unwrapAll[F[_]](value: F[Type]): F[A] = value.asInstanceOf[F[A]]

  /**
   * Converts an instance of the underlying type to an instance of the newtype.
   *
   * If there is a `def assertion` (see [[assert]]), the value will be checked
   * at compile-time.
   */
  def apply(value: A): Type = macro zio.prelude.Macros.wrap_impl[A, Type]

  /**
   * Converts multiple instances of the underlying type to [[NonEmptyChunk]] of
   * instances of the newtype.
   *
   * If there is a `def assertion` (see [[assert]]), each value will be checked
   * at compile-time.
   */
  def apply(value: A, values: A*): NonEmptyChunk[Type] = macro zio.prelude.Macros.applyMany_impl[A, Type]

  def make(value: A): Validation[String, Type] = macro zio.prelude.Macros.make_impl[A, Type]

  def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[Type]] =
    macro zio.prelude.Macros.makeAll_impl[F, A, Type]

  /**
   * This method is used to generate Newtype that can be validated at
   * compile-time. This must wrap a [[Assertion]] and be assigned to
   * `def assertion`.
   *
   * For example, here is a refined Newtype for Natural numbers. Natural
   * numbers are whole numbers greater than or equal to 0.
   *
   * {{{
   * import zio.prelude.Subtype
   * import zio.prelude.Assertion._
   *
   * type Natural = Natural.Type
   * object Natural extends Subtype[Int] {
   *   def assertion = assert(greaterThanOrEqualTo(0))
   * }
   * }}}
   *
   * With this `assertion` defined, `Natural.apply` will check literal values
   * at compile-time, failing with an error message if the Assertion is not
   * satisfied.
   *
   * `Natural(-10)` would render "`-10 failed to satisfy greaterThanOrEqualTo(10)`"
   *
   * IMPORTANT: Due to the macro machinery powering this feature, you must be
   * sure to NOT ANNOTATE `def assertion` with a type (`QuotedAssertion`). If
   * you do so, the macro will not be able to run the provided assertion at
   * compile-time and will fail with a message containing this very same
   * information.
   */
  def assert(assertion: Assertion[A]): QuotedAssertion[A] = macro zio.prelude.Macros.assert_impl[A]

  /**
   * Converts an instance of a type parameterized on the underlying type
   * to an instance of a type parameterized on the newtype. For example,
   * this could be used to convert a list of instances of the underlying
   * type to a list of instances of the newtype.
   *
   * Due to macro limitations, this method cannot with refined newtype and
   * will thus issue a compiler error if you attempt to do so.
   */
  def wrapAll[F[_]](value: F[A]): F[Type] = macro zio.prelude.Macros.wrapAll_impl[F, A, Type]

}

object Newtype {
  def assert[A](assertion: Assertion[A]): QuotedAssertion[A] = macro zio.prelude.Macros.assert_impl[A]

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype, ignoring any [[Assertion]].
   */
  def unsafeWrap[T <: Newtype[_]](newtype: T)(value: newtype.Wrapped): newtype.Type = {
    val _ = newtype
    value.asInstanceOf[newtype.Type]
  }

  /**
   * Converts an instance of a type parameterized on the underlying type
   * to an instance of a type parameterized on the newtype, ignoring any
   * [[Assertion]]. For example, this could be used to convert a list of
   * instances of the underlying type to a list of instances of the newtype.
   */
  def unsafeWrapAll[F[_], A, T <: Newtype[A]](newtype: T, value: F[A]): F[T#Type] = {
    val _ = newtype
    value.asInstanceOf[F[T#Type]]
  }
}
