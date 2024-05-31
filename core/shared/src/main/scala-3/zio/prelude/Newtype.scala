package zio.prelude

import zio.NonEmptyChunk

import scala.quoted.*
import zio.prelude.ConsoleUtils.*
import scala.reflect.ClassTag

abstract class NewtypeCustom[A] {

  type Type

  /**
   * Function that will used to check runtime values before
   * lifting them into the newtype. Should be consistent with `validateInline`.
   */
  protected def validate(value: A): Either[AssertionError, Unit]

  /**
   * Function that will be used to check compile-time values before
   * lifting them into the newtype. Should be consistent with `validate`.
   */
  protected inline def validateInline(inline value: A): Unit

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype.
   */
  inline def apply(inline a1: A): Type = {
    validateInline(a1)
    a1.asInstanceOf[Type]
  }

  inline def apply(inline a1: A, inline a2: A): NonEmptyChunk[Type] = {
    validateInline(a1)
    validateInline(a2)
    NonEmptyChunk(a1, a2).asInstanceOf[NonEmptyChunk[Type]]
  }

  inline def apply(inline a1: A, inline a2: A, inline a3: A): NonEmptyChunk[Type] = {
    validateInline(a1)
    validateInline(a2)
    validateInline(a3)
    NonEmptyChunk(a1, a2, a3).asInstanceOf[NonEmptyChunk[Type]]
  }

  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A): NonEmptyChunk[Type] = {
    validateInline(a1)
    validateInline(a2)
    validateInline(a3)
    validateInline(a4)
    NonEmptyChunk(a1, a2, a3, a4).asInstanceOf[NonEmptyChunk[Type]]
  }

  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A, inline a5: A): NonEmptyChunk[Type] = {
    validateInline(a1)
    validateInline(a2)
    validateInline(a3)
    validateInline(a4)
    validateInline(a5)
    NonEmptyChunk(a1, a2, a3, a4, a5).asInstanceOf[NonEmptyChunk[Type]]
  }

  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A, inline a5: A, inline a6: A): NonEmptyChunk[Type] = {
    validateInline(a1)
    validateInline(a2)
    validateInline(a3)
    validateInline(a4)
    validateInline(a5)
    validateInline(a6)
    NonEmptyChunk(a1, a2, a3, a4, a5, a6).asInstanceOf[NonEmptyChunk[Type]]
  }

  /**
   * Derives an instance of a type class for the new type given an instance
   * of the type class for the underlying type. The caller is responsible for
   * the type class being a valid instance for the new type.
   */
  def derive[TypeClass[_]](implicit instance: TypeClass[A]): TypeClass[Type] =
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
  def wrap(value: A): Type = value.asInstanceOf[Type]

  /**
   * Converts an instance of a type parameterized on the underlying type to an
   * instance of a type parameterized on the newtype.
   */
  def wrapAll[F[_]](value: F[A]): F[Type] = value.asInstanceOf[F[Type]]

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

  def make(value: A): Validation[String, Type] =
    Validation.fromEitherNonEmptyChunk(
      validate(value).left.map(e => NonEmptyChunk.fromCons(e.toNel(value.toString)))
    ).as(value.asInstanceOf[Type])

  def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[Type]] =
    ForEach[F].forEach(value) { value =>
      Validation.fromEitherNonEmptyChunk(
        validate(value).left.map(e => NonEmptyChunk.fromCons(e.toNel(value.toString)))
      )
    }.as(value.asInstanceOf[F[Type]])

  implicit def classTag(implicit underlying: ClassTag[A]): ClassTag[Type] = underlying.asInstanceOf[ClassTag[Type]]
}

abstract class SubtypeCustom[A] extends NewtypeCustom[A] {
  override type Type <: A
}

abstract class Newtype[A] extends NewtypeCustom[A] {
  def assertion: Assertion[A] = Assertion.anything

  protected def validate(value: A): Either[AssertionError, Unit] = assertion(value)

  protected inline def validateInline(inline value: A): Unit = ${Macros.validateInlineImpl[A]('assertion, 'value)}

}
