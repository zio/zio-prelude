package zio.prelude

import zio.NonEmptyChunk

import scala.quoted.*
import zio.prelude.ConsoleUtils.*
import scala.reflect.ClassTag

/**
 * The Scala 3 base class for newtypes whose validation is written by hand
 * instead of with an [[Assertion]]. A subclass implements `validate`, which
 * `make` and `makeAll` run on values at run time, and `validateInline`, which
 * `apply` runs on its arguments at compile time. The usual implementation
 * delegates both to a [[Validator]]. [[Newtype]] extends this class and
 * implements both members from its `assertion`.
 *
 * {{{
 * // no-check: refers to a Validator, which must live in another source file
 * object Palindrome extends NewtypeCustom[String] {
 *   protected def validate(value: String) =
 *     PalindromeValidator.validate(value)
 *
 *   protected inline def validateInline(inline value: String) =
 *     \${ PalindromeValidator.validateInlineImpl('value) }
 * }
 * type Palindrome = Palindrome.Type
 * }}}
 */
abstract class NewtypeCustom[A] {

  /**
   * The newtype. Declare a type alias for it next to the object, for example
   * `type Meter = Meter.Type`.
   */
  type Type

  /**
   * Checks a value at run time; `make` and `makeAll` call it. It must accept
   * the same values as `validateInline`.
   */
  protected def validate(value: A): Either[AssertionError, Unit]

  /**
   * Checks an argument of `apply` at compile time, usually by splicing a
   * [[Validator]]'s `validateInlineImpl`. It must accept the same values as
   * `validate`.
   */
  protected inline def validateInline(inline value: A): Unit

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype. When the newtype defines an `assertion`, the argument must be a
   * literal: it is checked at compile time, and a value that fails the check,
   * or an argument the compiler can't evaluate, such as a variable, does not
   * compile. Use `make` to check a value at run time.
   */
  inline def apply(inline a1: A): Type = {
    validateInline(a1)
    a1.asInstanceOf[Type]
  }

  /**
   * Converts several instances of the underlying type to a `NonEmptyChunk` of
   * instances of the newtype. When the newtype defines an `assertion`, every
   * argument must be a literal and is checked at compile time. On Scala 2 this
   * method takes any number of values, and the error lists every argument that
   * fails the check. On Scala 3 there are overloads for two to six values, and
   * the error names the first argument that fails.
   */
  inline def apply(inline a1: A, inline a2: A): NonEmptyChunk[Type] = {
    validateInline(a1)
    validateInline(a2)
    NonEmptyChunk(a1, a2).asInstanceOf[NonEmptyChunk[Type]]
  }

  /**
   * Converts several instances of the underlying type to a `NonEmptyChunk` of
   * instances of the newtype. When the newtype defines an `assertion`, every
   * argument must be a literal and is checked at compile time. On Scala 2 this
   * method takes any number of values, and the error lists every argument that
   * fails the check. On Scala 3 there are overloads for two to six values, and
   * the error names the first argument that fails.
   */
  inline def apply(inline a1: A, inline a2: A, inline a3: A): NonEmptyChunk[Type] = {
    validateInline(a1)
    validateInline(a2)
    validateInline(a3)
    NonEmptyChunk(a1, a2, a3).asInstanceOf[NonEmptyChunk[Type]]
  }

  /**
   * Converts several instances of the underlying type to a `NonEmptyChunk` of
   * instances of the newtype. When the newtype defines an `assertion`, every
   * argument must be a literal and is checked at compile time. On Scala 2 this
   * method takes any number of values, and the error lists every argument that
   * fails the check. On Scala 3 there are overloads for two to six values, and
   * the error names the first argument that fails.
   */
  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A): NonEmptyChunk[Type] = {
    validateInline(a1)
    validateInline(a2)
    validateInline(a3)
    validateInline(a4)
    NonEmptyChunk(a1, a2, a3, a4).asInstanceOf[NonEmptyChunk[Type]]
  }

  /**
   * Converts several instances of the underlying type to a `NonEmptyChunk` of
   * instances of the newtype. When the newtype defines an `assertion`, every
   * argument must be a literal and is checked at compile time. On Scala 2 this
   * method takes any number of values, and the error lists every argument that
   * fails the check. On Scala 3 there are overloads for two to six values, and
   * the error names the first argument that fails.
   */
  inline def apply(inline a1: A, inline a2: A, inline a3: A, inline a4: A, inline a5: A): NonEmptyChunk[Type] = {
    validateInline(a1)
    validateInline(a2)
    validateInline(a3)
    validateInline(a4)
    validateInline(a5)
    NonEmptyChunk(a1, a2, a3, a4, a5).asInstanceOf[NonEmptyChunk[Type]]
  }

  /**
   * Converts several instances of the underlying type to a `NonEmptyChunk` of
   * instances of the newtype. When the newtype defines an `assertion`, every
   * argument must be a literal and is checked at compile time. On Scala 2 this
   * method takes any number of values, and the error lists every argument that
   * fails the check. On Scala 3 there are overloads for two to six values, and
   * the error names the first argument that fails.
   */
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
   * Converts an instance of a type parameterized on the underlying type
   * to an instance of a type parameterized on the newtype. For example,
   * this could be used to convert a list of instances of the underlying
   * type to a list of instances of the newtype.
   *
   * It does not check the assertion. On Scala 2, calling it on a newtype that
   * defines an `assertion` does not compile; on Scala 3 it compiles and skips
   * the check. Use `makeAll` to check the elements.
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

  /**
   * Checks `value` against the assertion at run time. Returns the value as
   * the newtype in a successful [[Validation]], or a failed one with a message
   * such as `-1 did not satisfy greaterThanOrEqualTo(0)` for each condition
   * that the value fails. Without an `assertion`, every value succeeds.
   */
  def make(value: A): Validation[String, Type] =
    Validation.fromEitherNonEmptyChunk(
      validate(value).left.map(e => NonEmptyChunk.fromCons(e.toNel(value.toString)))
    ).as(value.asInstanceOf[Type])

  /**
   * Checks every element of `value` against the assertion at run time. Returns
   * the whole collection as a collection of the newtype when every element
   * passes, or a failed [[Validation]] with the messages of all the elements
   * that fail.
   */
  def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[Type]] =
    ForEach[F].forEach(value) { value =>
      Validation.fromEitherNonEmptyChunk(
        validate(value).left.map(e => NonEmptyChunk.fromCons(e.toNel(value.toString)))
      )
    }.as(value.asInstanceOf[F[Type]])

  /**
   * The `ClassTag` of the newtype, which is the `ClassTag` of the underlying
   * type. For example, an `Array` of a newtype over `Double` is created as an
   * `Array[Double]`.
   */
  implicit def classTag(implicit underlying: ClassTag[A]): ClassTag[Type] = underlying.asInstanceOf[ClassTag[Type]]
}

/**
 * A [[NewtypeCustom]] whose `Type` is a subtype of the underlying type `A`:
 * the hand-validated counterpart of [[Subtype]] on Scala 3.
 */
abstract class SubtypeCustom[A] extends NewtypeCustom[A] {
  override type Type <: A
}

/**
 * The base class for newtypes. A newtype wraps an existing type and is
 * represented by the same value at runtime, but the Scala compiler treats it
 * as a distinct type. Newtypes increase type safety when modeling a domain,
 * for example by giving `Meter` and `Foot` separate types. They also give a
 * type that has more than one lawful instance of a type class one newtype per
 * instance, as the predefined `And` and `Or` newtypes do for `Boolean`.
 *
 * To create a newtype, define an object that extends `Newtype[A]`, where `A`
 * is the underlying type, and a type alias for its `Type`:
 *
 * {{{
 * object Meter extends Newtype[Double]
 * type Meter = Meter.Type
 *
 * val distance: Meter = Meter(3.4)
 * val meters: Double  = Meter.unwrap(distance) // 3.4
 * }}}
 *
 * `apply` and `wrapAll` convert values of the underlying type to the newtype,
 * and `unwrap`, `unwrapAll` and pattern matching with `unapply` convert them
 * back. These conversions are casts and don't allocate a wrapper object.
 * Because `Type` is an abstract type, a newtype over a primitive type such as
 * `Double` is stored boxed, like a value of a generic type.
 *
 * Apart from these conversions, `Meter` and `Double` are unrelated types: a
 * `Meter` can't be passed where a `Double` is expected, and it has none of the
 * methods of `Double`. Extend [[Subtype]] instead to keep them.
 *
 * A smart newtype also defines an `assertion` that the underlying value must
 * satisfy. `apply` checks literal arguments at compile time and does not
 * compile for an argument the compiler can't evaluate, such as a variable.
 * `make` checks any value at run time and returns a [[Validation]]. The
 * definition of `assertion` differs between Scala 2 and Scala 3:
 *
 * {{{
 * // scala-2-only
 * import zio.prelude.Assertion._
 *
 * object Natural extends Newtype[Int] {
 *   // do not annotate the type of `assertion`
 *   override def assertion = assert(greaterThanOrEqualTo(0))
 * }
 * type Natural = Natural.Type
 *
 * val ten: Natural = Natural(10)
 * // Natural(-99) does not compile: -99 did not satisfy greaterThanOrEqualTo(0)
 *
 * val parsed: Validation[String, Natural] = Natural.make(-1)
 * // Failure(Chunk(),NonEmptyChunk(-1 did not satisfy greaterThanOrEqualTo(0)))
 * }}}
 *
 * {{{
 * // scala-3-only
 * import zio.prelude.Assertion._
 *
 * object Natural extends Newtype[Int] {
 *   override inline def assertion = greaterThanOrEqualTo(0)
 * }
 * type Natural = Natural.Type
 *
 * val ten: Natural = Natural(10)
 * // Natural(-99) does not compile: -99 did not satisfy greaterThanOrEqualTo(0)
 *
 * val parsed: Validation[String, Natural] = Natural.make(-1)
 * // Failure(Chunk(),NonEmptyChunk(-1 did not satisfy greaterThanOrEqualTo(0)))
 * }}}
 *
 * @see [[https://zio.dev/zio-prelude/newtypes/ New Types]]
 */
abstract class Newtype[A] extends NewtypeCustom[A] {

  /**
   * The assertion that values of the newtype must satisfy. The default
   * accepts every value. A smart newtype overrides it: on Scala 2 with
   * `override def assertion = assert(...)` and no type annotation, on Scala 3
   * with `override inline def assertion = ...`.
   */
  def assertion: Assertion[A] = Assertion.anything

  protected def validate(value: A): Either[AssertionError, Unit] = assertion(value)

  protected inline def validateInline(inline value: A): Unit = ${Macros.validateInlineImpl[A]('assertion, 'value)}

}
