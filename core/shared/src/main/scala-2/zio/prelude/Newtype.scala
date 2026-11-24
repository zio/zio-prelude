package zio.prelude

import zio.NonEmptyChunk

import scala.reflect.ClassTag

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
abstract class Newtype[A] extends NewtypeVersionSpecific {

  /**
   * The underlying type `A`.
   */
  type Wrapped = A

  /**
   * The assertion that values of the newtype must satisfy. The default
   * accepts every value. A smart newtype overrides it: on Scala 2 with
   * `override def assertion = assert(...)` and no type annotation, on Scala 3
   * with `override inline def assertion = ...`.
   */
  def assertion: QuotedAssertion[A] = new QuotedAssertion[A] {
    def run(value: A): Either[AssertionError, Unit] = Assertion.anything(value)
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
   * Converts an instance of the underlying type to an instance of the
   * newtype. When the newtype defines an `assertion`, the argument must be a
   * literal: it is checked at compile time, and a value that fails the check,
   * or an argument the compiler can't evaluate, such as a variable, does not
   * compile. Use `make` to check a value at run time.
   */
  def apply(value: A): Type = macro zio.prelude.Macros.wrap_impl[A, Type]

  /**
   * Converts several instances of the underlying type to a `NonEmptyChunk` of
   * instances of the newtype. When the newtype defines an `assertion`, every
   * argument must be a literal and is checked at compile time. On Scala 2 this
   * method takes any number of values, and the error lists every argument that
   * fails the check. On Scala 3 there are overloads for two to six values, and
   * the error names the first argument that fails.
   */
  def apply(value: A, values: A*): NonEmptyChunk[Type] = macro zio.prelude.Macros.applyMany_impl[A, Type]

  /**
   * Checks `value` against the assertion at run time. Returns the value as
   * the newtype in a successful [[Validation]], or a failed one with a message
   * such as `-1 did not satisfy greaterThanOrEqualTo(0)` for each condition
   * that the value fails. Without an `assertion`, every value succeeds.
   */
  def make(value: A): Validation[String, Type] = macro zio.prelude.Macros.make_impl[A, Type]

  /**
   * Checks every element of `value` against the assertion at run time. Returns
   * the whole collection as a collection of the newtype when every element
   * passes, or a failed [[Validation]] with the messages of all the elements
   * that fail.
   */
  def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[Type]] =
    macro zio.prelude.Macros.makeAll_impl[F, A, Type]

  /**
   * Turns an `Assertion` into the value of `def assertion` on Scala 2, so
   * that `apply` can check literal arguments at compile time and `make` can
   * check values at run time.
   *
   * For example, a smart newtype for natural numbers, the whole numbers
   * greater than or equal to 0:
   *
   * {{{
   * import zio.prelude.Assertion._
   *
   * object Natural extends Subtype[Int] {
   *   override def assertion = assert(greaterThanOrEqualTo(0))
   * }
   * type Natural = Natural.Type
   *
   * val two: Natural = Natural(2)
   * }}}
   *
   * `Natural(-10)` does not compile, and the error contains
   * `-10 did not satisfy greaterThanOrEqualTo(0)`.
   *
   * The assertion must be written inside the `assert` call, built from the
   * constructors of `Assertion` with literal arguments. An assertion stored
   * in another `val` can't be read at compile time, and `apply` then fails
   * with "You have defined your Assertion in a way that cannot be read at
   * compile-time."
   *
   * Do not annotate `def assertion` with a type (`QuotedAssertion`). With the
   * annotation, `apply` fails to compile with a message that asks you to
   * remove it.
   */
  def assert(assertion: Assertion[A]): QuotedAssertion[A] = macro zio.prelude.Macros.assert_impl[A]

  /**
   * Turns a custom check into the value of `def assertion` on Scala 2, for
   * conditions that the constructors of `Assertion` can't express. Prefer
   * `assert` when an `Assertion` fits.
   *
   * {{{
   * // scala-2-only
   * object Palindrome extends Newtype[String] {
   *   override def assertion = assertCustom { (s: String) =>
   *     if (s.reverse == s) Right(()) else Left(AssertionError.failure("isPalindrome"))
   *   }
   * }
   *
   * val result = Palindrome.make("abc")
   * // Failure(Chunk(),NonEmptyChunk(abc did not satisfy isPalindrome))
   * }}}
   *
   * The function is compiled and run inside the compiler with
   * `whitebox.Context.eval`, so it can call only code that is already
   * compiled, such as the standard library and zio-prelude. Calling an object
   * from the same compilation run fails the macro expansion with a
   * `java.lang.ClassNotFoundException`.
   *
   * Do not annotate `def assertion` with a type (`QuotedAssertion`). With the
   * annotation, `apply` fails to compile with a message that asks you to
   * remove it.
   */
  def assertCustom(f: A => Either[AssertionError, Unit]): QuotedAssertion[A] =
    macro zio.prelude.Macros.assertCustom_impl[A]

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
  def wrapAll[F[_]](value: F[A]): F[Type] = macro zio.prelude.Macros.wrapAll_impl[F, A, Type]

  /**
   * The `ClassTag` of the newtype, which is the `ClassTag` of the underlying
   * type. For example, an `Array` of a newtype over `Double` is created as an
   * `Array[Double]`.
   */
  implicit def classTag(implicit underlying: ClassTag[A]): ClassTag[Type] = underlying.asInstanceOf[ClassTag[Type]]
}

/**
 * Operations on newtypes that skip the assertion. The compile-time error of a
 * smart newtype's `apply` suggests `unsafeWrap` and `unsafeWrapAll` for
 * arguments that are not literals.
 */
object Newtype {

  /**
   * Turns an `Assertion` into a `QuotedAssertion`, like the `assert` method
   * of a newtype object. The result can be assigned to `def assertion`, which
   * must not have a type annotation.
   */
  def assert[A](assertion: Assertion[A]): QuotedAssertion[A] = macro zio.prelude.Macros.assert_impl[A]

  /**
   * Converts an instance of the underlying type to an instance of the
   * newtype, ignoring any Assertion.
   */
  def unsafeWrap[T <: Newtype[_]](newtype: T)(value: newtype.Wrapped): newtype.Type = {
    val _ = newtype
    value.asInstanceOf[newtype.Type]
  }

  /**
   * Converts an instance of a type parameterized on the underlying type
   * to an instance of a type parameterized on the newtype, ignoring any
   * Assertion. For example, this could be used to convert a list of
   * instances of the underlying type to a list of instances of the newtype.
   */
  def unsafeWrapAll[F[_], A, T <: Newtype[A]](newtype: T, value: F[A]): F[T#Type] = {
    val _ = newtype
    value.asInstanceOf[F[T#Type]]
  }

  /**
   * Holds the `ClassTag` of the underlying type of the newtype object type
   * `A`. `Newtype.classTag` uses it to build a `ClassTag` for `N#Type`.
   */
  trait ClassTagWrapper[-A] {

    /**
     * The `ClassTag` of the underlying type.
     */
    def classTag: ClassTag[_]
  }

  /**
   * Instances of [[Newtype.ClassTagWrapper]].
   */
  object ClassTagWrapper {

    /**
     * Summons the implicit `ClassTagWrapper` for `A`.
     */
    def apply[A](implicit ev: ClassTagWrapper[A]): ClassTagWrapper[A] = ev

    /**
     * A `ClassTagWrapper` for a newtype over `A`, built from the `ClassTag`
     * of `A`.
     */
    implicit def classTagWrapperForNewtype[A](implicit underlying: ClassTag[A]): ClassTagWrapper[Newtype[A]] =
      new ClassTagWrapper[Newtype[A]] {
        def classTag: ClassTag[_] = underlying
      }
  }

  /**
   * The `ClassTag` of the type `N#Type` of a newtype object type `N`, which
   * is the `ClassTag` of its underlying type.
   */
  implicit def classTag[N <: Newtype[_]: ClassTagWrapper]: ClassTag[N#Type] =
    ClassTagWrapper[N].classTag.asInstanceOf[ClassTag[N#Type]]
}
