/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.prelude

import zio.test.Assertion

/**
 * The `Newtype` module provides functionality for creating zero overhead
 * newtypes. Newtypes wrap an existing type and have the same representation as
 * the underlying type at runtime but are treated as distinct types by the
 * Scala compiler. Newtypes can be used to increase type safety in modeling a
 * domain, for example by creating separate types for `Meter` and `Foot`. They
 * can also be used to provide coherent instances for types that can support
 * more than one version of a typeclass. For example, the `And` and `Or`
 * types can be used to distinguish boolean conjunction and disjunction.
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
 * Finally, it is possible to create newtypes that require smart constructors
 * by extending `NewTypeSmart` or `SubtypeSmart`. In this case we must provide
 * an additional argument to the class we are extending indicating how to
 * validate an instance of the underlying type. For example, let's create a
 * newtype for natural numbers, which must be equal to or greater than zero.
 *
 * {{{
 * object Natural extends NewtypeSmart[Int](isGreaterThanEqualTo(0))
 * type Natural = Natural.Type
 * }}}
 *
 * In this case, attempting to convert an integer to a natural number will
 * return a `Validation[String, Natural]` that will either contain a`Natural`
 * if the integer was equal to or greater than zero or a string with a
 * descriptive error message if the condition was not satisfied.
 */
private[prelude] sealed trait NewtypeModule {

  def newtype[A]: Newtype[A]

  def newtypeSmart[A](assertion: Assertion[A]): NewtypeSmart[A]

  def subtype[A]: Subtype[A]

  def subtypeSmart[A](assertion: Assertion[A]): SubtypeSmart[A]

  private[this] type Id[+A] = A

  private implicit val IdForEach: ForEach[Id] =
    new ForEach[Id] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: Id[A])(f: A => G[B]): G[Id[B]] =
        f(fa)
    }

  sealed trait Newtype[A] {
    type Type

    /**
     * Converts an instance of the underlying type to an instance of the
     * newtype.
     */
    def apply(value: A): Type = wrap(value)

    /**
     * Allows pattern matching on newtype instances to convert them back to
     * instances of the underlying type.
     */
    def unapply(value: Type): Some[A] = Some(unwrap(value))

    /**
     * Converts an instance of the underlying type to an instance of the
     * newtype.
     */
    def wrap(value: A): Type = wrapAll[Id](value)

    /**
     * Converts an instance of the newtype back to an instance of the
     * underlying type.
     */
    def unwrap(value: Type): A = unwrapAll[Id](value)

    /**
     * Converts an instance of a type parameterized on the underlying type
     * to an instance of a type parameterized on the newtype. For example,
     * this could be used to convert a list of instances of the underlying
     * type to a list of instances of the newtype.
     */
    def wrapAll[F[_]](value: F[A]): F[Type]

    /**
     * Converts an instance of a type parameterized on the newtype back to an
     * instance of a type parameterized on the underlying type. For example,
     * this could be used to convert a list of instances of the newtype back
     * to a list of instances of the underlying type.
     */
    def unwrapAll[F[_]](value: F[Type]): F[A]
  }

  sealed trait NewtypeSmart[A] {
    type Type

    /**
     * Converts an instance of the underlying type to an instance of the
     * newtype.
     */
    protected def apply(value: A): Type = wrap(value)

    /**
     * Attempts to convert an instance of the underlying type to an instance
     * of the newtype, returning a `Validation` containing either a valid
     * instance of the newtype or a string message describing why the instance
     * was invalid.
     */
    def make(value: A): Validation[String, Type] = makeAll[Id](value)

    /**
     * Allows pattern matching on newtype instances to convert them back to
     * instances of the underlying type.
     */
    def unapply(value: Type): Some[A] = Some(unwrap(value))

    /**
     * Converts an instance of the underlying type to an instance of the
     * newtype.
     */
    protected def wrap(value: A): Type = wrapAll[Id](value)

    /**
     * Converts an instance of the newtype back to an instance of the
     * underlying type.
     */
    def unwrap(value: Type): A = unwrapAll[Id](value)

    /**
     * Attempts to convert a collection of instances of the underlying type to
     * a collection of instances of the newtype, returning a `Validation`
     * containing either a collection of valid instances of the newtype or an
     * accumulation of validation errors.
     */
    def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[Type]]

    /**
     * Converts an instance of a type parameterized on the underlying type
     * to an instance of a type parameterized on the newtype. For example,
     * this could be used to convert a list of instances of the underlying
     * type to a list of instances of the newtype.
     */
    protected def wrapAll[F[_]](value: F[A]): F[Type]

    /**
     * Converts an instance of a type parameterized on the newtype back to an
     * instance of a type parameterized on the underlying type. For example,
     * this could be used to convert a list of instances of the newtype back
     * to a list of instances of the underlying type.
     */
    def unwrapAll[F[_]](value: F[Type]): F[A]

    private[prelude] def unsafeWrapAll[F[_]](value: F[A]): F[Type]
  }

  sealed trait Subtype[A] extends Newtype[A] {
    type Type <: A
  }

  sealed trait SubtypeSmart[A] extends NewtypeSmart[A] {
    type Type <: A
  }
}

private[prelude] object NewtypeModule {
  val instance: NewtypeModule =
    new NewtypeModule {
      def newtype[A]: Newtype[A] =
        new Newtype[A] {
          type Type = A

          def wrapAll[F[_]](value: F[A]): F[Type] = value

          def unwrapAll[F[_]](value: F[Type]): F[A] = value
        }

      def newtypeSmart[A](assertion: Assertion[A]): NewtypeSmart[A] =
        new NewtypeSmart[A] {
          type Type = A

          def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[A]] =
            ForEach[F].forEach[({ type lambda[+A] = Validation[String, A] })#lambda, A, A](value)(
              Validation.fromAssert(_)(assertion)
            )

          protected def wrapAll[F[_]](value: F[A]): F[A] = value

          def unwrapAll[F[_]](value: F[A]): F[A] = value

          private[prelude] def unsafeWrapAll[F[_]](value: F[A]): F[A] = value
        }

      def subtype[A]: Subtype[A] =
        new Subtype[A] {
          type Type = A

          def wrapAll[F[_]](value: F[A]): F[Type] = value.asInstanceOf[F[Type]]

          def unwrapAll[F[_]](value: F[Type]): F[A] = value.asInstanceOf[F[A]]
        }

      def subtypeSmart[A](assertion: Assertion[A]): SubtypeSmart[A] =
        new SubtypeSmart[A] {
          type Type = A

          def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[A]] =
            ForEach[F].forEach[({ type lambda[+A] = Validation[String, A] })#lambda, A, A](value)(
              Validation.fromAssert(_)(assertion)
            )

          protected def wrapAll[F[_]](value: F[A]): F[A] = value

          def unwrapAll[F[_]](value: F[A]): F[A] = value

          private[prelude] def unsafeWrapAll[F[_]](value: F[A]): F[A] = value
        }
    }
}

trait NewtypeExports {
  import NewtypeModule._

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
  abstract class Newtype[A] extends instance.Newtype[A] {
    val newtype: instance.Newtype[A] = instance.newtype[A]

    trait Tag extends Any
    type Type = newtype.Type with Tag

    def wrapAll[F[_]](value: F[A]): F[Type] = newtype.wrapAll(value).asInstanceOf[F[Type]]

    def unwrapAll[F[_]](value: F[Type]): F[A] = newtype.unwrapAll(value.asInstanceOf[F[newtype.Type]])
  }

  /**
   * The class of objects corresponding to newtypes with smart constructors
   * where not all instances of the underlying type are valid instances of the
   * newtype. Users should implement an object that extends this class to
   * create their own newtypes, specifying `A` as the underlying type to wrap
   * and an assertion that valid instances of the underlying type should
   * satisfy.
   *
   * {{{
   * object Natural extends NewtypeSmart[Int](isGreaterThanEqualTo(0))
   * type Natural = Natural.Type
   * }}}
   */
  abstract class NewtypeSmart[A](assertion: Assertion[A]) extends instance.NewtypeSmart[A] {
    val newtype: instance.NewtypeSmart[A] = instance.newtypeSmart[A](assertion)

    trait Tag extends Any
    type Type = newtype.Type with Tag

    def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[Type]] =
      newtype.makeAll(value).asInstanceOf[Validation[String, F[Type]]]

    protected def wrapAll[F[_]](value: F[A]): F[Type] = unsafeWrapAll(value)

    def unwrapAll[F[_]](value: F[Type]): F[A] = newtype.unwrapAll(value.asInstanceOf[F[newtype.Type]])

    private[prelude] def unsafeWrapAll[F[_]](value: F[A]): F[Type] = newtype.unsafeWrapAll(value).asInstanceOf[F[Type]]
  }

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
  abstract class Subtype[A] extends instance.Subtype[A] {
    val subtype: instance.Subtype[A] = instance.subtype[A]

    trait Tag extends Any
    type Type = subtype.Type with Tag

    def wrapAll[F[_]](value: F[A]): F[Type] = subtype.wrapAll(value).asInstanceOf[F[Type]]

    def unwrapAll[F[_]](value: F[Type]): F[A] = subtype.unwrapAll(value.asInstanceOf[F[subtype.Type]])
  }

  /**
   * The class of objects corresponding to subtypes with smart constructors
   * where not all instances of the underlying type are valid instances of the
   * subtype. Users should implement an object that extends this class to
   * create their own subtypes, specifying `A` as the underlying type to wrap
   * and an assertion that valid instances of the underlying type should
   * satisfy.
   *
   * {{{
   * object Natural extends SubtypeSmart[Int](isGreaterThanEqualTo(0))
   * type Natural = Natural.Type
   * }}}
   */
  abstract class SubtypeSmart[A](assertion: Assertion[A]) extends instance.SubtypeSmart[A] {
    val subtype: instance.SubtypeSmart[A] = instance.subtypeSmart[A](assertion)

    trait Tag extends Any
    type Type = subtype.Type with Tag

    def makeAll[F[+_]: ForEach](value: F[A]): Validation[String, F[Type]] =
      subtype.makeAll(value).asInstanceOf[Validation[String, F[Type]]]

    protected def wrapAll[F[_]](value: F[A]): F[Type] = unsafeWrapAll(value)

    def unwrapAll[F[_]](value: F[Type]): F[A] = subtype.unwrapAll(value.asInstanceOf[F[subtype.Type]])

    private[prelude] def unsafeWrapAll[F[_]](value: F[A]): F[Type] = subtype.unsafeWrapAll(value).asInstanceOf[F[Type]]
  }
}
