package zio.prelude

import zio.NonEmptyChunk

import scala.quoted.*
import zio.prelude.ConsoleUtils.*

abstract class NewtypeCustom[A] {

  type Base
  trait Tag extends Any
  type Type = Base with Tag

  def assertion: Assertion[A] = Assertion.anything

  inline def validateInline(inline value: A): A

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype.
   */
  inline def apply(inline a1: A): Type =
    validateInline(a1).asInstanceOf[Type]

  inline def apply(inline a1: A, inline a2: A): NonEmptyChunk[Type] =
    NonEmptyChunk(apply(a1), apply(a2))

  inline def apply(inline a1: A, inline a2: A, inline a3: A): NonEmptyChunk[Type] =
    NonEmptyChunk(apply(a1), apply(a2), apply(a3))

  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A): NonEmptyChunk[Type] =
    NonEmptyChunk(apply(a1), apply(a2), apply(a3), apply(a4))

  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A, inline a5: A): NonEmptyChunk[Type] =
    NonEmptyChunk(apply(a1), apply(a2), apply(a3), apply(a4), apply(a5))

  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A, inline a5: A, inline a6: A): NonEmptyChunk[Type] =
    NonEmptyChunk(apply(a1), apply(a2), apply(a3), apply(a4), apply(a5), apply(a6))

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
   * Converts an instance of a type parameterized on the underlying type to an
   * instance of a type parameterized on the newtype.
   */
  protected def wrapAll[F[_]](value: F[A]): F[Type] = value.asInstanceOf[F[Type]]

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
      assertion.apply(value).left.map(e => NonEmptyChunk.fromCons(e.toNel(value.toString)))
    ).as(value.asInstanceOf[Type])


  def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[Type]] =
    ForEach[F].forEach(value) { value =>
      Validation.fromEitherNonEmptyChunk(
        assertion.apply(value).left.map(e => NonEmptyChunk.fromCons(e.toNel(value.toString)))
      )
    }.as(value.asInstanceOf[F[Type]])
}

abstract class SubtypeCustom[A] extends NewtypeCustom[A] {
  type Base <: A
}

abstract class Newtype[A] extends NewtypeCustom[A] {

  inline def validateInline(inline value: A): A =
    ${Macros.validateInlineImpl[A]('assertion, 'value)}

}
