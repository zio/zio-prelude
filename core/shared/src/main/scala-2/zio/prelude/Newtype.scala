package zio.prelude

import zio.NonEmptyChunk

/**
 * The `Newtype` module provides functionality for creating zero overhead
 * newtypes. Newtypes wrap an existing type and have the same representation as
 * the underlying type at runtime but are treated as distinct types by the
 * Scala compiler. Newtypes can be used to increase type safety in modeling a
 * domain, for example by creating separate types for `Meter` and `Foot`.
 * Additionally, Smart Newtypes allow for compile-time validation of values,
 * e.g., ensuring an Int is 4 digits long (a PIN) or that a String matches a
 * particular Regex (an Email).
 *
 * Newtypes can also be used to provide coherent instances for types that can
 * support more than one version of a typeclass. For example, the `And` and
 * `Or` types can be used to distinguish boolean conjunction and disjunction.
 *
 * To create a newtype, simply implement an object which extends `Newtype[A]`,
 * where `A` is the underlying type you are wrapping. Then implement a type
 * alias for `Type` in the object you create.
 *
 * {{{
 * object Meter extends Newtype[Double]
 * type Meter = Meter.Type
 * }}}
 *
 * This creates a newtype `Meter` that wraps an underlying `Double` value.
 *
 * We can convert the underlying type to the newtype using the `wrap` method on
 * the object we created, or convert a collection of values of the underlying
 * type using `wrapAll`. Similarly we can convert a newtype back to the
 * underlying value using `unwrap` and `unwrapAll`. We can also wrap a value in
 * a newtype using the `apply` method of the object we created or pattern match
 * to recover the underlying value using its `unapply` method. None of these
 * methods incur any runtime overhead in doing these conversions.
 *
 * {{{
 * implicit class MeterSyntax(private val self: Meter) extends AnyVal {
 *   def +(that: Meter): Meter =
 *     Meter.wrap(Meter.unwrap(self) + Meter.unwrap(that))
 * }
 *
 * val x = Meter(3.4)
 * val y = Meter(4.3)
 * val z = x + y
 * }}}
 *
 * In the example above, `Meter` is completely independent of `Double`. Outside
 * of the methods we create to convert between them, the two types are
 * completely unrelated from the perspective of the Scala compiler. This can be
 * valuable because it fully hides the underlying type. For example, we cannot
 * accidentally add a `Meter` and a `Foot`. On the other hand `Meter` also does
 * not have any of the methods defined on it that are defined for `Double`
 * values so we have to re-implement them ourselves or wrap and unwrap each
 * time.
 *
 * It is also possible to create newtypes that are subtypes of existing types.
 * To do this, just implement your newtype the same we as described above but
 * extends `Subtype[A]`, where `A` is the underlying value you are wrapping.
 *
 * {{{
 * object And extends Subtype[Boolean]
 * type And = And.Type
 *
 * object Or extends Subtype[Boolean]
 * type Or = Or.Type
 *
 * implicit object AndInstance extends Associative[And] with Identity[And] {
 *   def combine(l: => And, r: => And): And =
 *     And(l && r)
 *   val identity: And =
 *     And(true)
 * }
 *
 * implicit object OrInstance extends Associative[Or] with Identity[Or] {
 *   def combine(l: => Or, r: => Or): Or =
 *     Or(l || r)
 *   val identity: Or =
 *     Or(false)
 * }
 *
 * def foldMap[A, B](as: List[A])(f: A => B)(implicit B: Associative[B] with Identity[B]) =
 *   as.foldLeft(B.identity)((b, a) => B.combine(b, f(a)))
 *
 * def exists[A](as: List[A])(f: A => Boolean): Boolean =
 *   Or.unwrap(foldMap(as)(a => Or(f(a))))
 *
 * def forall[A](as: List[A])(f: A => Boolean): Boolean =
 *   And.unwrap(foldMap(as)(a => And(f(a))))
 * }}}
 *
 * Notice how `And` and `Or` are distinct types that each can have their own
 * coherent typeclass instances, but operations on `Boolean` such as `&&` and
 * `||` are still available on them. These methods can still return the
 * underlying type, so this technique is most useful for creating coherent
 * typeclasses instances rather than type safe domain specific languages.
 *
 * Finally, it is possible to create Smart Newtypes that can be validated at
 * compile-time when constructed with a literal value, or at run-time with a
 * dynamic value, returning a [[Validation]]. In this case we must define an
 * additional `def assertion` method on the Object, indicating how to validate
 * an instance of the underlying type. For example, let's create a newtype for
 * natural numbers, which must be equal to or greater than zero. (Note that the
 * syntax differs between Scala 2 and 3 due to changes in the macro API).
 *
 * {{{
 * import zio.prelude.Assertion.greaterThanOrEqualTo
 *
 * type Natural = Natural.Type
 * object Natural extends Newtype[Int] {
 *   // Scala 2 — be sure you DO NOT give a type annotation to `assertion`
 *   def assertion = assert {
 *     greaterThanOrEqualTo(0)
 *   }
 *
 *   // Scala 3
 *   override inline def assertion: Assertion[Int] =
 *     greaterThanOrEqualTo(0)
 * }
 * }}}
 *
 * In this case, attempting to convert an integer to a natural number will
 * return a `Natural` that will either return a`Natural` if the integer was
 * equal to or greater than zero or a fail to compile with a descriptive error
 * message if the condition was not satisfied.
 *
 * {{{
 *   val compiles = Natural(10)
 *   val fails    = Natural(-99) // -99 does not satisfy greaterThanOrEqualTo(0)
 * }}}
 *
 * If you need to supply a variable or other run-time validation to a Smart
 * Newtype, use the `make` method instead, which will return a [[Validation]].
 *
 * {{{
 *   val result: Validation[String, Natural] = Natural.make(10)
 * }}}
 */

/**
 * The class of objects corresponding to subtypes. Users should implement an
 * object that extends this class to create their own subtypes, specifying
 * `A` as the underlying type to wrap.
 *
 * {{{
 * object And extends Subtype[Boolean]
 * type And = And.Type
 * }}}
 */

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
abstract class Newtype[A] extends NewtypeVersionSpecific {
  type Wrapped = A

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
