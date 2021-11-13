package zio.prelude

import zio.NonEmptyChunk

import scala.quoted._
import zio.prelude.ConsoleUtils.*
import scala.compiletime.{error, codeOf}

abstract class NewtypeSmart[A] {

  type Base
  trait Tag extends Any
  type Type = Base & Tag

  inline def validateInline(inline value: A): A

  def validate(value: A): Either[AssertionError, A]

  /**
   * Derives an instance of a type class for the new type given an instance
   * of the type class for the underlying type. The caller is responsible for
   * the type class being a valid instance for the new type.
   */
  protected def derive[TypeClass[_]](implicit instance: TypeClass[A]): TypeClass[Type] =
    instance.asInstanceOf[TypeClass[Type]]

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype.
   */
  inline def apply(inline a1: A): Type =
    validateInline(a1).asInstanceOf[Type]

  inline def apply(inline a1: A, inline a2: A): NonEmptyChunk[Type] =
    NonEmptyChunk(apply(a1): Type, apply(a2): Type)

  inline def apply(inline a1: A, inline a2: A, inline a3: A): NonEmptyChunk[Type] = {
    validateInline(a1)
    validateInline(a2)
    validateInline(a3)
    NonEmptyChunk(a1, a2, a3).asInstanceOf[NonEmptyChunk[Type]]
  }

  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A): NonEmptyChunk[Type] =
    NonEmptyChunk(apply(a1): Type, apply(a2): Type, apply(a3): Type, apply(a4): Type)

  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A, inline a5: A): NonEmptyChunk[Type] =
    NonEmptyChunk(apply(a1): Type, apply(a2): Type, apply(a3): Type, apply(a4): Type, apply(a5): Type)

  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A, inline a5: A, inline a6: A): NonEmptyChunk[Type] =
    NonEmptyChunk(apply(a1): Type, apply(a2): Type, apply(a3): Type, apply(a4): Type, apply(a5): Type, apply(a6): Type)

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
   * Converts an instance of a collection of the underlying type to an instance of a collection of the
   * newtype. Ignores the assertion.
   */
  protected def wrapAll[F[_]](values: F[A]): F[Type] = values.asInstanceOf[F[Type]]

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
}

abstract class Newtype[A] extends NewtypeSmart[A] {

  override inline def validateInline(inline value: A): A =
    value

  override final def validate(value: A): Either[AssertionError, A] =
    Right(value)

}

abstract class SubtypeSmart[A] extends NewtypeSmart[A] {

  type Base <: A


}

abstract class Subtype[A] extends Newtype[A] {

  type Base <: A


}
