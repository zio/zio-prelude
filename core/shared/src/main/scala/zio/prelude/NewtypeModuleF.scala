/*
 * Copyright 2020-2023 John A. De Goes and the ZIO Contributors
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

/**
 * The `NewtypeF` module provides functionality for creating newtypes that are
 * parameterized on some type `A`. See the documentation on `Newtype` for a
 * more general discussion of newtypes, their use cases, and creating and
 * working with newtypes. Unlike a `Newtype`, a newtype created by this module
 * is parameterized on some type `A`. For example, we could try to use
 * unparameterized newtypes to distinguish integer addition from
 * multiplication:
 *
 * {{{
 * object IntSum extends Newtype[Int]
 * type IntSum = IntSum.Type
 *
 * object IntMult extends Newtype[Int]
 * type IntMult = IntMult.Type
 * }}}
 *
 * However, what happens if we want to similarly distinguish addition and
 * multiplication for longs and other numeric values? We could implement
 * separate newtypes for each of them (e.g. `LongSum`, `LongMult`) but this
 * involves significant boilerplate and removes our ability to abstract over
 * addition in general, separate from the specific type of numeric values we
 * are adding.
 *
 * We can improve this by using the `NewtypeF` module. We define our newtype
 * exactly as we did before but we extend `NewtypeF` or `SubtypeF` and we do
 * not specify any type parameter. We then define our type alias as a type
 * parameterized on some type `A`, and give it instances for different types:
 *
 * {{{
 * object Sum extends SubtypeF
 * type Sum[A] = Sum.Type[A]
 *
 * implicit val IntSum: Identity[Sum[Int]]   = Identity.make(Sum(0), (l, r) => Sum(l + r))
 * implicit val LongSum: Identity[Sum[Long]] = Identity.make(Sum(0L), (l, r) => Sum(l + r))
 *
 * def sum[A](as: List[A])(implicit A: Identity[Sum[A]]): A =
 *   Sum.unwrap(Sum.wrapAll(as).foldLeft(A.identity)((b, a) => A.combine(b, a)))
 *
 * sum(List(1, 2, 3))    // 6
 * sum(List(1L, 2L, 3L)) // 6L
 * }}}
 *
 * Just as with other newtypes, `apply`, `wrap`, and `wrapAll` are available to
 * convert instances of the underlying type to instances of the newtype and
 * `unapply`, `unwrap`, and `unwrapAll` can be used to convert instances of the
 * newtype back to instances of the underlying type. Variants for subtypes are
 * also available.
 */
private[prelude] sealed trait NewtypeModuleF {

  /**
   * The implementation of parameterized newtypes that `NewtypeF` delegates to.
   */
  def newtypeF: NewtypeF

  /**
   * The implementation of parameterized subtypes that `SubtypeF` delegates to.
   */
  def subtypeF: SubtypeF

  private[this] type Id[+A] = A

  private implicit val IdForEach: ForEach[Id] =
    new ForEach[Id] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: Id[A])(f: A => G[B]): G[Id[B]] =
        f(fa)
    }

  /**
   * The operations of a parameterized newtype.
   */
  sealed trait NewtypeF {

    /**
     * The newtype, parameterized on the type `A` it wraps.
     */
    type Type[+A]

    /**
     * Converts an instance of the underlying type to an instance of the
     * newtype.
     */
    def apply[A](value: A): Type[A] = wrap(value)

    /**
     * Allows pattern matching on newtype instances to convert them back to
     * instances of the underlying type.
     */
    def unapply[A](value: Type[A]): Some[A] = Some(unwrap(value))

    /**
     * Converts an instance of the underlying type to an instance of the
     * newtype.
     */
    def wrap[A](value: A): Type[A] = wrapAll[Id, A](value)

    /**
     * Converts an instance of the newtype back to an instance of the
     * underlying type.
     */
    def unwrap[A](value: Type[A]): A = unwrapAll[Id, A](value)

    /**
     * Converts an instance of a type parameterized on the underlying type
     * to an instance of a type parameterized on the newtype. For example,
     * this could be used to convert a list of instances of the underlying
     * type to a list of instances of the newtype.
     */
    def wrapAll[F[_], A](value: F[A]): F[Type[A]]

    /**
     * Converts an instance of a type parameterized on the newtype back to an
     * instance of a type parameterized on the underlying type. For example,
     * this could be used to convert a list of instances of the newtype back
     * to a list of instances of the underlying type.
     */
    def unwrapAll[F[_], A](value: F[Type[A]]): F[A]
  }

  /**
   * The operations of a parameterized smart newtype, which checks the values
   * it wraps. It is deprecated, and no public class extends it.
   */
  @deprecated("deprecated", "1.0.0-RC8")
  sealed trait NewtypeSmartF[A[_]] {

    /**
     * The newtype of `A[x]`.
     */
    type Type[x]

    /**
     * Converts an instance of the underlying type to an instance of the
     * newtype.
     */
    protected def apply[x](value: A[x]): Type[x] = wrap(value)

    /**
     * Attempts to convert an instance of the underlying type to an instance
     * of the newtype, returning a `Validation` containing either a valid
     * instance of the newtype or a string message describing why the instance
     * was invalid.
     */
    def make[x](value: A[x]): Validation[String, Type[x]] = makeAll[Id, x](value)

    /**
     * Allows pattern matching on newtype instances to convert them back to
     * instances of the underlying type.
     */
    def unapply[x](value: Type[x]): Some[A[x]] = Some(unwrap(value))

    /**
     * Converts an instance of the underlying type to an instance of the
     * newtype.
     */
    protected def wrap[x](value: A[x]): Type[x] = wrapAll[Id, x](value)

    /**
     * Converts an instance of the newtype back to an instance of the
     * underlying type.
     */
    def unwrap[x](value: Type[x]): A[x] = unwrapAll[Id, x](value)

    /**
     * Attempts to convert a collection of instances of the underlying type to
     * a collection of instances of the newtype, returning a `Validation`
     * containing either a collection of valid instances of the newtype or an
     * accumulation of validation errors.
     */
    def makeAll[F[+_]: ForEach, x](value: F[A[x]]): Validation[String, F[Type[x]]]

    /**
     * Converts an instance of a type parameterized on the underlying type
     * to an instance of a type parameterized on the newtype. For example,
     * this could be used to convert a list of instances of the underlying
     * type to a list of instances of the newtype.
     */
    protected def wrapAll[F[_], x](value: F[A[x]]): F[Type[x]]

    /**
     * Converts an instance of a type parameterized on the newtype back to an
     * instance of a type parameterized on the underlying type. For example,
     * this could be used to convert a list of instances of the newtype back
     * to a list of instances of the underlying type.
     */
    def unwrapAll[F[_], x](value: F[Type[x]]): F[A[x]]

    private[prelude] def unsafeWrapAll[F[_], x](value: F[A[x]]): F[Type[x]]
  }

  /**
   * The operations of a parameterized subtype, whose `Type[A]` is a subtype of
   * `A`.
   */
  sealed trait SubtypeF extends NewtypeF {

    /**
     * The subtype, parameterized on the type `A` it wraps.
     */
    type Type[+A] <: A
  }

  /**
   * The subtype variant of `NewtypeSmartF`, whose `Type[x]` is a subtype of
   * `A[x]`. It is deprecated, and no public class extends it.
   */
  @deprecated("deprecated", "1.0.0-RC8")
  sealed trait SubtypeSmartF[A[_]] extends NewtypeSmartF[A] {

    /**
     * The subtype of `A[x]`.
     */
    type Type[x] <: A[x]
  }
}

private[prelude] object NewtypeModuleF {
  val instance: NewtypeModuleF =
    new NewtypeModuleF {
      def newtypeF: this.NewtypeF =
        new this.NewtypeF {
          type Type[+A] = A

          def wrapAll[F[_], A](value: F[A]): F[A] = value

          def unwrapAll[F[_], A](value: F[A]): F[A] = value
        }

      def subtypeF: this.SubtypeF =
        new this.SubtypeF {
          type Type[+A] = A

          def wrapAll[F[_], A](value: F[A]): F[A] = value

          def unwrapAll[F[_], A](value: F[A]): F[A] = value
        }
    }
}

/**
 * Adds the parameterized newtype classes `NewtypeF` and `SubtypeF` to the
 * `zio.prelude` package.
 */
trait NewtypeFExports {
  import NewtypeModuleF._

  /**
   * The class of objects corresponding to parameterized newtypes. Users should
   * implement an object that extends this class to create their own
   * parameterized newtypes. A `Type[A]` is distinct from `A`: it can't be
   * passed where an `A` is expected.
   *
   * {{{
   * object Label extends NewtypeF
   * type Label[A] = Label.Type[A]
   *
   * val label: Label[Int] = Label(42)
   * val raw: Int          = Label.unwrap(label) // 42
   * }}}
   */
  abstract class NewtypeF extends instance.NewtypeF {

    /**
     * The implementation that this newtype delegates to.
     */
    val newtypeF: instance.NewtypeF =
      instance.newtypeF

    /**
     * The newtype, parameterized on the type `A` it wraps.
     */
    type Type[+A] = newtypeF.Type[A]

    /**
     * Converts an instance of a type parameterized on the underlying type
     * to an instance of a type parameterized on the newtype, without
     * traversing it.
     */
    def wrapAll[F[_], A](value: F[A]): F[Type[A]] =
      newtypeF.wrapAll(value)

    /**
     * Converts an instance of a type parameterized on the newtype back to an
     * instance of a type parameterized on the underlying type, without
     * traversing it.
     */
    def unwrapAll[F[_], A](value: F[Type[A]]): F[A] =
      newtypeF.unwrapAll(value)
  }

  /**
   * The class of objects corresponding to parameterized subtypes. Users should
   * implement an object that extends this class to create their own
   * parameterized subtypes. A `Type[A]` is a subtype of `A`, so it keeps the
   * methods of `A`.
   *
   * {{{
   * object Total extends SubtypeF
   * type Total[A] = Total.Type[A]
   *
   * val total: Total[Int] = Total(40)
   * val more: Int         = total + 2 // 42
   * }}}
   */
  abstract class SubtypeF extends instance.SubtypeF {

    /**
     * The implementation that this subtype delegates to.
     */
    val subtypeF: instance.SubtypeF =
      instance.subtypeF

    /**
     * The subtype, parameterized on the type `A` it wraps.
     */
    type Type[+A] = subtypeF.Type[A]

    /**
     * Converts an instance of a type parameterized on the underlying type
     * to an instance of a type parameterized on the subtype, without
     * traversing it.
     */
    def wrapAll[F[_], A](value: F[A]): F[Type[A]] =
      subtypeF.wrapAll(value)

    /**
     * Converts an instance of a type parameterized on the subtype back to an
     * instance of a type parameterized on the underlying type, without
     * traversing it.
     */
    def unwrapAll[F[_], A](value: F[Type[A]]): F[A] =
      subtypeF.unwrapAll(value)
  }
}
