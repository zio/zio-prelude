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
 * working with newtypes. Unlike the newtypes in that module, the newtypes
 * created by this module are parameterized on some type `A`. For example, we
 * could try to use unparameterized newtypes to distinguish integer addition
 * from multiplication:
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
 * exactly as we did before but we extends `NewtypeF` or `SubtypeF` and we do
 * not specify any type parameter. We then define our type alias as a type
 * parameterized on some type `A`:
 *
 * {{{
 * object Sum extends SubtypeF
 * type Sum[A] = Sum.Type[A]
 *
 * object Prod extends SubtypeF
 * type Prod[A] = Prod.Type[A]
 * }}}
 *
 * We can then parameterize our newtype on different types:
 *
 * {{{
 * implicit val IntInstance: Associative[Sum[Int]] with Identity[Sum[Int]] = ???
 * implicit val LongInstance: Associative[Sum[Long]] with Identity[Sum[Long]] = ???
 *
 * def sum[A](as: List[A])(implicit A: Associative[Sum[A]] with Identity[Sum[A]]): A =
 *   Sum.unwrap(Sum.wrapAll(as).foldLeft(A.identity)((b, a) => A.combine(b, a)))
 * }}}
 *
 * sum(List(1, 2, 3))
 * sum(List(1L, 2L, 3L))
 * }}}
 *
 * Just as with other newtypes, `apply`, `wrap`, and `wrapAll` are available to
 * convert instances of the underlying type to instances of the newtype and
 * `unapply`, `unwrap`, and `unwrapAll` can be used to convert instances of the
 * newtype back to instances of the underlying type. Variants for subtypes are
 * also available.
 */
private[prelude] sealed trait NewtypeModuleF {

  def newtypeF: NewtypeF

  def subtypeF: SubtypeF

  private[this] type Id[+A] = A

  private implicit val IdForEach: ForEach[Id] =
    new ForEach[Id] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: Id[A])(f: A => G[B]): G[Id[B]] =
        f(fa)
    }

  sealed trait NewtypeF {
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

  @deprecated("deprecated", "1.0.0-RC8")
  sealed trait NewtypeSmartF[A[_]] {
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

  sealed trait SubtypeF extends NewtypeF {
    type Type[+A] <: A
  }

  @deprecated("deprecated", "1.0.0-RC8")
  sealed trait SubtypeSmartF[A[_]] extends NewtypeSmartF[A] {
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

trait NewtypeFExports {
  import NewtypeModuleF._

  /**
   * The class of objects corresponding to parameterized newtypes. Users should
   * implement an object that extends this class to create their own
   * parameterized newtypes
   *
   * {{{
   * object Sum extends NewtypeF
   * type Sum[A] = Sum.Type[A]
   * }}}
   */
  abstract class NewtypeF extends instance.NewtypeF {
    val newtypeF: instance.NewtypeF =
      instance.newtypeF

    type Type[+A] = newtypeF.Type[A]

    def wrapAll[F[_], A](value: F[A]): F[Type[A]] =
      newtypeF.wrapAll(value)

    def unwrapAll[F[_], A](value: F[Type[A]]): F[A] =
      newtypeF.unwrapAll(value)
  }

  /**
   * The class of objects corresponding to parameterized subtypes. Users should
   * implement an object that extends this class to create their own
   * parameterized subtypes
   *
   * {{{
   * object Sum extends SubtypeF
   * type Sum[A] = Sum.Type[A]
   * }}}
   */
  abstract class SubtypeF extends instance.SubtypeF {
    val subtypeF: instance.SubtypeF =
      instance.subtypeF

    type Type[+A] = subtypeF.Type[A]

    def wrapAll[F[_], A](value: F[A]): F[Type[A]] =
      subtypeF.wrapAll(value)

    def unwrapAll[F[_], A](value: F[Type[A]]): F[A] =
      subtypeF.unwrapAll(value)
  }
}
