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

package zio.prelude.coherent

import zio._
import zio.prelude._

/**
 * An instance that is `AssociativeBoth`, `DeriveEqual` and `Invariant` at the same time, so that one implicit value can
 * supply all of them.
 */
trait AssociativeBothDeriveEqualInvariant[F[_]] extends AssociativeBoth[F] with DeriveEqual[F] with Invariant[F]

/**
 * Derives [[AssociativeBothDeriveEqualInvariant]] instances from separate instances.
 */
object AssociativeBothDeriveEqualInvariant {

  /**
   * Combines the implicit `AssociativeBoth`, `DeriveEqual` and `Invariant` instances into one
   * [[AssociativeBothDeriveEqualInvariant]] that delegates to them.
   */
  implicit def derive[F[_]](implicit
    associativeBoth0: AssociativeBoth[F],
    deriveEqual0: DeriveEqual[F],
    invariant0: Invariant[F]
  ): AssociativeBothDeriveEqualInvariant[F] =
    new AssociativeBothDeriveEqualInvariant[F] {
      def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] = associativeBoth0.both(fa, fb)
      def derive[A: Equal]: Equal[F[A]]                   = deriveEqual0.derive
      def invmap[A, B](f: A <=> B): F[A] <=> F[B]         = invariant0.invmap(f)
    }
}

/**
 * An instance that is `AssociativeEither`, `DeriveEqual` and `Invariant` at the same time, so that one implicit value
 * can supply all of them.
 */
trait AssociativeEitherDeriveEqualInvariant[F[_]] extends AssociativeEither[F] with DeriveEqual[F] with Invariant[F]

/**
 * Derives [[AssociativeEitherDeriveEqualInvariant]] instances from separate instances.
 */
object AssociativeEitherDeriveEqualInvariant {

  /**
   * Combines the implicit `AssociativeEither`, `DeriveEqual` and `Invariant` instances into one
   * [[AssociativeEitherDeriveEqualInvariant]] that delegates to them.
   */
  implicit def derive[F[_]](implicit
    associativeEither0: AssociativeEither[F],
    deriveEqual0: DeriveEqual[F],
    invariant0: Invariant[F]
  ): AssociativeEitherDeriveEqualInvariant[F] =
    new AssociativeEitherDeriveEqualInvariant[F] {
      def derive[A: Equal]: Equal[F[A]]                           = deriveEqual0.derive
      def either[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]] = associativeEither0.either(fa, fb)
      def invmap[A, B](f: A <=> B): F[A] <=> F[B]                 = invariant0.invmap(f)
    }
}

/**
 * An instance that is `Associative` and `Equal` at the same time, so that one implicit value can supply both.
 */
trait AssociativeEqual[A] extends Associative[A] with Equal[A]

/**
 * Derives [[AssociativeEqual]] instances from separate instances.
 */
object AssociativeEqual {

  /**
   * Combines the implicit `Associative` and `Equal` instances into one [[AssociativeEqual]] that delegates to them.
   */
  implicit def derive[A](implicit associative0: Associative[A], equal0: Equal[A]): AssociativeEqual[A] =
    new AssociativeEqual[A] {
      def combine(l: => A, r: => A): A              = associative0.combine(l, r)
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * An instance that is `AssociativeFlatten`, `Covariant` and `DeriveEqual` at the same time, so that one implicit value
 * can supply all of them.
 */
trait AssociativeFlattenCovariantDeriveEqual[F[+_]] extends AssociativeFlatten[F] with Covariant[F] with DeriveEqual[F]

/**
 * Derives [[AssociativeFlattenCovariantDeriveEqual]] instances from separate instances.
 */
object AssociativeFlattenCovariantDeriveEqual {

  /**
   * Combines the implicit `AssociativeFlatten`, `Covariant` and `DeriveEqual` instances into one
   * [[AssociativeFlattenCovariantDeriveEqual]] that delegates to them.
   */
  implicit def derive[F[+_]](implicit
    associativeFlatten0: AssociativeFlatten[F],
    covariant0: Covariant[F],
    deriveEqual0: DeriveEqual[F]
  ): AssociativeFlattenCovariantDeriveEqual[F] =
    new AssociativeFlattenCovariantDeriveEqual[F] {
      def derive[A: Equal]: Equal[F[A]]      =
        deriveEqual0.derive
      def flatten[A](ffa: F[F[A]]): F[A]     =
        associativeFlatten0.flatten(ffa)
      def map[A, B](f: A => B): F[A] => F[B] =
        covariant0.map(f)
    }
}

/**
 * An instance that is `CommutativeBoth`, `DeriveEqual` and `Invariant` at the same time, so that one implicit value can
 * supply all of them.
 */
trait CommutativeBothDeriveEqualInvariant[F[_]] extends AssociativeBothDeriveEqualInvariant[F] with CommutativeBoth[F]

/**
 * Derives [[CommutativeBothDeriveEqualInvariant]] instances from separate instances.
 */
object CommutativeBothDeriveEqualInvariant {

  /**
   * Combines the implicit `CommutativeBoth`, `DeriveEqual` and `Invariant` instances into one
   * [[CommutativeBothDeriveEqualInvariant]] that delegates to them.
   */
  implicit def derive[F[_]](implicit
    commutativeBoth0: CommutativeBoth[F],
    deriveEqual0: DeriveEqual[F],
    invariant0: Invariant[F]
  ): CommutativeBothDeriveEqualInvariant[F] =
    new CommutativeBothDeriveEqualInvariant[F] {
      def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] = commutativeBoth0.both(fa, fb)
      def derive[A: Equal]: Equal[F[A]]                   = deriveEqual0.derive
      def invmap[A, B](f: A <=> B): F[A] <=> F[B]         = invariant0.invmap(f)
    }
}

/**
 * An instance that is `CommutativeEither`, `DeriveEqual` and `Invariant` at the same time, so that one implicit value
 * can supply all of them.
 */
trait CommutativeEitherDeriveEqualInvariant[F[_]]
    extends AssociativeEitherDeriveEqualInvariant[F]
    with CommutativeEither[F]

/**
 * Derives [[CommutativeEitherDeriveEqualInvariant]] instances from separate instances.
 */
object CommutativeEitherDeriveEqualInvariant {

  /**
   * Combines the implicit `CommutativeEither`, `DeriveEqual` and `Invariant` instances into one
   * [[CommutativeEitherDeriveEqualInvariant]] that delegates to them.
   */
  implicit def derive[F[_]](implicit
    commutativeEither0: CommutativeEither[F],
    deriveEqual0: DeriveEqual[F],
    invariant0: Invariant[F]
  ): CommutativeEitherDeriveEqualInvariant[F] =
    new CommutativeEitherDeriveEqualInvariant[F] {
      def derive[A: Equal]: Equal[F[A]]                           = deriveEqual0.derive
      def either[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]] = commutativeEither0.either(fa, fb)
      def invmap[A, B](f: A <=> B): F[A] <=> F[B]                 = invariant0.invmap(f)
    }
}

/**
 * An instance that is `Commutative` and `Equal` at the same time, so that one implicit value can supply both.
 */
trait CommutativeEqual[A] extends AssociativeEqual[A] with Commutative[A]

/**
 * Derives [[CommutativeEqual]] instances from separate instances.
 */
object CommutativeEqual {

  /**
   * Combines the implicit `Commutative` and `Equal` instances into one [[CommutativeEqual]] that delegates to them.
   */
  implicit def derive[A](implicit commutative0: Commutative[A], equal0: Equal[A]): CommutativeEqual[A] =
    new CommutativeEqual[A] {
      def combine(l: => A, r: => A): A              = commutative0.combine(l, r)
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * An instance that is `Commutative` and `Identity` at the same time, so that one implicit value can supply both.
 */
trait CommutativeIdentity[A] extends Commutative[A] with Identity[A]

/**
 * Derives [[CommutativeIdentity]] instances from separate instances.
 */
object CommutativeIdentity {

  /**
   * Combines the implicit `Commutative` and `Identity` instances into one [[CommutativeIdentity]] that delegates to
   * them.
   */
  implicit def derive[A](implicit commutative0: Commutative[A], identity0: Identity[A]): CommutativeIdentity[A] =
    new CommutativeIdentity[A] {
      def combine(l: => A, r: => A): A = commutative0.combine(l, r)
      def identity: A                  = identity0.identity
    }
}

/**
 * An instance that is `Covariant` and `DeriveEqual` at the same time, so that one implicit value can supply both.
 */
trait CovariantDeriveEqual[F[+_]] extends Covariant[F] with DeriveEqual[F]

/**
 * Derives [[CovariantDeriveEqual]] instances from separate instances.
 */
object CovariantDeriveEqual {

  /**
   * Combines the implicit `Covariant` and `DeriveEqual` instances into one [[CovariantDeriveEqual]] that delegates to
   * them.
   */
  implicit def derive[F[+_]](implicit covariant0: Covariant[F], deriveEqual0: DeriveEqual[F]): CovariantDeriveEqual[F] =
    new CovariantDeriveEqual[F] {
      def derive[A: Equal]: Equal[F[A]]      =
        deriveEqual0.derive
      def map[A, B](f: A => B): F[A] => F[B] =
        covariant0.map(f)
    }
}

/**
 * An instance that is `Contravariant` and `DeriveEqual` at the same time, so that one implicit value can supply both.
 */
trait ContravariantDeriveEqual[F[-_]] extends Contravariant[F] with DeriveEqual[F]

/**
 * Derives [[ContravariantDeriveEqual]] instances from separate instances.
 */
object ContravariantDeriveEqual {

  /**
   * Combines the implicit `Contravariant` and `DeriveEqual` instances into one [[ContravariantDeriveEqual]] that
   * delegates to them.
   */
  implicit def derive[F[-_]](implicit
    contravariant0: Contravariant[F],
    deriveEqual0: DeriveEqual[F]
  ): ContravariantDeriveEqual[F] =
    new ContravariantDeriveEqual[F] {
      def derive[A: Equal]: Equal[F[A]]            =
        deriveEqual0.derive
      def contramap[A, B](f: B => A): F[A] => F[B] =
        contravariant0.contramap(f)
    }
}

/**
 * An instance that is `Covariant`, `DeriveEqual` and `IdentityFlatten` at the same time, so that one implicit value can
 * supply all of them.
 */
trait CovariantDeriveEqualIdentityFlatten[F[+_]]
    extends IdentityFlatten[F]
    with AssociativeFlattenCovariantDeriveEqual[F]

/**
 * Derives [[CovariantDeriveEqualIdentityFlatten]] instances from separate instances.
 */
object CovariantDeriveEqualIdentityFlatten {

  /**
   * Combines the implicit `Covariant`, `DeriveEqual` and `IdentityFlatten` instances into one
   * [[CovariantDeriveEqualIdentityFlatten]] that delegates to them.
   */
  implicit def derive[F[+_]](implicit
    covariant0: Covariant[F],
    deriveEqual0: DeriveEqual[F],
    identityFlatten0: IdentityFlatten[F]
  ): CovariantDeriveEqualIdentityFlatten[F] =
    new CovariantDeriveEqualIdentityFlatten[F] {
      def any: F[Any]                        =
        identityFlatten0.any
      def derive[A: Equal]: Equal[F[A]]      =
        deriveEqual0.derive
      def flatten[A](ffa: F[F[A]]): F[A]     =
        identityFlatten0.flatten(ffa)
      def map[A, B](f: A => B): F[A] => F[B] =
        covariant0.map(f)
    }
}

/**
 * An instance that is `Covariant` and `IdentityBoth` at the same time, so that one context bound can ask for both. It
 * adds `forEach`, `forEach_` and `collectM`, which run one `F` value for each element of a collection and combine them
 * with `both`.
 *
 * {{{
 * import zio.prelude._
 *
 * val all: Option[List[Int]] =
 *   CovariantIdentityBoth[Option].forEach(List(1, 2, 3))(n => Some(n * 10))
 * // Some(List(10, 20, 30))
 *
 * val failed: Option[List[Int]] =
 *   CovariantIdentityBoth[Option].forEach(List(1, 2, 3))(n => if (n == 2) None else Some(n))
 * // None
 * }}}
 */
trait CovariantIdentityBoth[F[+_]] extends Covariant[F] with IdentityBoth[F] { self =>
  private implicit val covariant: Covariant[F]       = self
  private implicit val identityBoth: IdentityBoth[F] = self

  /**
   * Applies `f` to each element of `in`, combines the results with `both`, and keeps the values that are `Some`, in a
   * collection of the same type as `in`.
   */
  def collectM[A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(f: A => F[Option[B]])(implicit
    bf: BuildFrom[Collection[A], B, Collection[B]]
  ): F[Collection[B]] =
    forEach[A, Option[B], Iterable](in)(f).map(_.flatten).map(bf.fromSpecific(in))

  /**
   * Applies `f` to each element of `in`, from first to last, and combines the results with `both` into one `F` of a
   * collection of the same type as `in`.
   */
  def forEach[A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(f: A => F[B])(implicit
    bf: BuildFrom[Collection[A], B, Collection[B]]
  ): F[Collection[B]] =
    in.foldLeft(List.empty[B].succeed)((fbs, a) => fbs.zipWith(f(a))((bs, b) => b :: bs)).map { bs =>
      val builder = bf.newBuilder(in)
      builder ++= bs.reverse
      builder.result()
    }

  /**
   * Applies `f` to each element of `in`, from first to last, combines the results with `both`, and discards the values.
   */
  def forEach_[A, B](in: Iterable[A])(f: A => F[Any]): F[Unit] =
    in.foldLeft(identityBoth.any)((bs, a) => bs *> f(a)).unit
}

/**
 * Summons and derives [[CovariantIdentityBoth]] instances.
 */
object CovariantIdentityBoth {

  /**
   * Summons the implicit `CovariantIdentityBoth` instance for `F`.
   */
  def apply[F[+_]](implicit covariantIdentityBoth: CovariantIdentityBoth[F]): CovariantIdentityBoth[F] =
    covariantIdentityBoth

  /**
   * Returns the implicit `IdentityBoth` instance itself when it is already a `CovariantIdentityBoth`, and otherwise
   * combines the implicit `Covariant` and `IdentityBoth` instances into one that delegates to them.
   */
  implicit def derive[F[+_]](implicit
    covariant: Covariant[F],
    identityBoth: IdentityBoth[F]
  ): CovariantIdentityBoth[F] =
    identityBoth match {
      case covariantIdentityBoth: CovariantIdentityBoth[F] => covariantIdentityBoth
      case _                                               => fromCovariantIdentityBoth(covariant, identityBoth)
    }

  private def fromCovariantIdentityBoth[F[+_]](
    covariant: Covariant[F],
    identityBoth: IdentityBoth[F]
  ): CovariantIdentityBoth[F] =
    new CovariantIdentityBoth[F] {
      def any: F[Any]                                     =
        identityBoth.any
      def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] =
        identityBoth.both(fa, fb)
      def map[A, B](f: A => B): F[A] => F[B]              =
        covariant.map(f)
    }
}

/**
 * An instance that is `DeriveEqual`, `IdentityBoth` and `Invariant` at the same time, so that one implicit value can
 * supply all of them.
 */
trait DeriveEqualIdentityBothInvariant[F[_]] extends AssociativeBothDeriveEqualInvariant[F] with IdentityBoth[F]

/**
 * Derives [[DeriveEqualIdentityBothInvariant]] instances from separate instances.
 */
object DeriveEqualIdentityBothInvariant {

  /**
   * Combines the implicit `DeriveEqual`, `IdentityBoth` and `Invariant` instances into one
   * [[DeriveEqualIdentityBothInvariant]] that delegates to them.
   */
  implicit def derive[F[_]](implicit
    deriveEqual0: DeriveEqual[F],
    identityBoth0: IdentityBoth[F],
    invariant0: Invariant[F]
  ): DeriveEqualIdentityBothInvariant[F] =
    new DeriveEqualIdentityBothInvariant[F] {
      val any: F[Any]                                     = identityBoth0.any
      def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] = identityBoth0.both(fa, fb)
      def derive[A: Equal]: Equal[F[A]]                   = deriveEqual0.derive
      def invmap[A, B](f: A <=> B): F[A] <=> F[B]         = invariant0.invmap(f)
    }
}

/**
 * An instance that is `DeriveEqual`, `IdentityEither` and `Invariant` at the same time, so that one implicit value can
 * supply all of them.
 */
trait DeriveEqualIdentityEitherInvariant[F[_]] extends AssociativeEitherDeriveEqualInvariant[F] with IdentityEither[F]

/**
 * Derives [[DeriveEqualIdentityEitherInvariant]] instances from separate instances.
 */
object DeriveEqualIdentityEitherInvariant {

  /**
   * Combines the implicit `DeriveEqual`, `IdentityEither` and `Invariant` instances into one
   * [[DeriveEqualIdentityEitherInvariant]] that delegates to them.
   */
  implicit def derive[F[_]](implicit
    deriveEqual0: DeriveEqual[F],
    identityEither0: IdentityEither[F],
    invariant0: Invariant[F]
  ): DeriveEqualIdentityEitherInvariant[F] =
    new DeriveEqualIdentityEitherInvariant[F] {
      def derive[A: Equal]: Equal[F[A]]                           = deriveEqual0.derive
      def either[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]] = identityEither0.either(fa, fb)
      def invmap[A, B](f: A <=> B): F[A] <=> F[B]                 = invariant0.invmap(f)
      val none: F[Nothing]                                        = identityEither0.none
    }
}

/**
 * An instance that is `DeriveEqual` and `NonEmptyForEach` at the same time, so that one implicit value can supply
 * both.
 */
trait DeriveEqualNonEmptyForEach[F[+_]] extends DeriveEqualForEach[F] with NonEmptyForEach[F]

/**
 * Derives [[DeriveEqualNonEmptyForEach]] instances from separate instances.
 */
object DeriveEqualNonEmptyForEach {

  /**
   * Combines the implicit `DeriveEqual` and `NonEmptyForEach` instances into one [[DeriveEqualNonEmptyForEach]] that
   * delegates to them.
   */
  implicit def derive[F[+_]](implicit
    deriveEqual0: DeriveEqual[F],
    nonEmptyForEach0: NonEmptyForEach[F]
  ): DeriveEqualNonEmptyForEach[F] =
    new DeriveEqualNonEmptyForEach[F] {
      def derive[A: Equal]: Equal[F[A]]                                                      =
        deriveEqual0.derive
      def forEach1[G[+_]: AssociativeBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        nonEmptyForEach0.forEach1(fa)(f)
    }
}

/**
 * An instance that is `DeriveEqual` and `ForEach` at the same time, so that one implicit value can supply both.
 */
trait DeriveEqualForEach[F[+_]] extends CovariantDeriveEqual[F] with ForEach[F]

/**
 * Derives [[DeriveEqualForEach]] instances from separate instances.
 */
object DeriveEqualForEach {

  /**
   * Combines the implicit `DeriveEqual` and `ForEach` instances into one [[DeriveEqualForEach]] that delegates to them.
   */
  implicit def derive[F[+_]](implicit
    deriveEqual0: DeriveEqual[F],
    forEach0: ForEach[F]
  ): DeriveEqualForEach[F] =
    new DeriveEqualForEach[F] {
      def derive[A: Equal]: Equal[F[A]]                                                  =
        deriveEqual0.derive
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        forEach0.forEach(fa)(f)
    }
}

/**
 * An instance that is `Equal` and `Identity` at the same time, so that one implicit value can supply both.
 */
trait EqualIdentity[A] extends AssociativeEqual[A] with Identity[A]

/**
 * Derives [[EqualIdentity]] instances from separate instances.
 */
object EqualIdentity {

  /**
   * Combines the implicit `Equal` and `Identity` instances into one [[EqualIdentity]] that delegates to them.
   */
  implicit def derive[A](implicit identity0: Identity[A], equal0: Equal[A]): EqualIdentity[A] =
    new EqualIdentity[A] {
      def combine(l: => A, r: => A): A              = identity0.combine(l, r)
      def identity: A                               = identity0.identity
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * An instance that is `Equal` and `Idempotent` at the same time, so that one implicit value can supply both.
 */
trait EqualIdempotent[A] extends AssociativeEqual[A] with Idempotent[A]

/**
 * Derives [[EqualIdempotent]] instances from separate instances.
 */
object EqualIdempotent {

  /**
   * Combines the implicit `Equal` and `Idempotent` instances into one [[EqualIdempotent]] that delegates to them.
   */
  implicit def derive[A](implicit idempotent0: Idempotent[A], equal0: Equal[A]): EqualIdempotent[A] =
    new EqualIdempotent[A] {
      def combine(l: => A, r: => A): A              = idempotent0.combine(l, r)
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * An instance that is `Equal` and `Inverse` at the same time, so that one implicit value can supply both.
 */
trait EqualInverse[A] extends EqualPartialInverse[A] with Inverse[A]

/**
 * Derives [[EqualInverse]] instances from separate instances.
 */
object EqualInverse {

  /**
   * Combines the implicit `Equal` and `Inverse` instances into one [[EqualInverse]] that delegates to them.
   */
  implicit def derive[A](implicit equal0: Equal[A], inverse0: Inverse[A]): EqualInverse[A] =
    new EqualInverse[A] {
      def combine(l: => A, r: => A): A                       = inverse0.combine(l, r)
      def identity: A                                        = inverse0.identity
      def inverse(l: => A, r: => A): A                       = inverse0.inverse(l, r)
      override protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * An instance that is `Equal` and `PartialInverse` at the same time, so that one implicit value can supply both.
 */
trait EqualPartialInverse[A] extends EqualIdentity[A] with PartialInverse[A]

/**
 * Derives [[EqualPartialInverse]] instances from separate instances.
 */
object EqualPartialInverse {

  /**
   * Combines the implicit `Equal` and `PartialInverse` instances into one [[EqualPartialInverse]] that delegates to
   * them.
   */
  implicit def derive[A](implicit equal0: Equal[A], inverse0: PartialInverse[A]): EqualPartialInverse[A] =
    new EqualPartialInverse[A] {
      def combine(l: => A, r: => A): A                       = inverse0.combine(l, r)
      def identity: A                                        = inverse0.identity
      def inverseOption(l: => A, r: => A): Option[A]         = inverse0.inverseOption(l, r)
      override protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

/**
 * An instance that is `Hash` and `PartialOrd` at the same time.
 */
trait HashPartialOrd[-A] extends Hash[A] with PartialOrd[A] { self =>

  /**
   * Returns an instance for `B` that hashes, compares and checks equality of `B` values by applying `f` and using this
   * instance on the results.
   */
  override def contramap[B](f: B => A): Hash[B] with PartialOrd[B] =
    new HashPartialOrd[B] {
      def hash(b: B): Int                                     = self.hash(f(b))
      protected def checkCompare(l: B, r: B): PartialOrdering = self.compare(f(l), f(r))
      override protected def checkEqual(l: B, r: B): Boolean  = self.equal(f(l), f(r))
    }
}

/**
 * Derives and constructs [[HashPartialOrd]] instances.
 */
object HashPartialOrd {

  /**
   * Combines the implicit `Hash` and `PartialOrd` instances into one [[HashPartialOrd]]. It hashes with the `Hash`
   * instance, and compares and checks equality with the `PartialOrd` instance.
   */
  implicit def derive[A](implicit hash0: Hash[A], ord0: PartialOrd[A]): HashPartialOrd[A] =
    new HashPartialOrd[A] {
      def hash(a: A): Int                                     = hash0.hash(a)
      protected def checkCompare(l: A, r: A): PartialOrdering = ord0.compare(l, r)
      override protected def checkEqual(l: A, r: A): Boolean  = ord0.equal(l, r)
    }

  /**
   * Constructs an instance from a `hash0` function, an `ord` function and an `equal0` function.
   * Equality checks call `equal0` instead of `ord`, so `equal0` can be cheaper than computing the ordering.
   */
  def make[A](hash0: A => Int, ord: (A, A) => PartialOrdering, equal0: (A, A) => Boolean): Hash[A] with PartialOrd[A] =
    new HashPartialOrd[A] {
      def hash(a: A): Int                                              = hash0(a)
      override protected def checkCompare(l: A, r: A): PartialOrdering = ord(l, r)
      override protected def checkEqual(l: A, r: A): Boolean           = equal0(l, r)
    }

  /**
   * Constructs an instance from a `hash0` function and an `ord` function.
   * Two values that are not the same reference are equal when `ord` returns `Ordering.Equals` for them.
   */
  def make[A](hash0: A => Int, ord: (A, A) => PartialOrdering): Hash[A] with PartialOrd[A] =
    new HashPartialOrd[A] {
      def hash(a: A): Int                                              = hash0(a)
      override protected def checkCompare(l: A, r: A): PartialOrdering = ord(l, r)
      override protected def checkEqual(l: A, r: A): Boolean           = ord(l, r) match {
        case Ordering.Equals => true
        case _               => false
      }
    }

}

/**
 * An instance that is `Hash` and `Ord` at the same time.
 */
trait HashOrd[-A] extends HashPartialOrd[A] with Ord[A] { self =>

  /**
   * Returns an instance for `B` that hashes, compares and checks equality of `B` values by applying `f` and using this
   * instance on the results.
   */
  final override def contramap[B](f: B => A): Hash[B] with Ord[B] =
    new HashOrd[B] {
      def hash(b: B): Int                                    = self.hash(f(b))
      protected def checkCompare(l: B, r: B): Ordering       = self.compare(f(l), f(r))
      override protected def checkEqual(l: B, r: B): Boolean = self.equal(f(l), f(r))
    }
}

/**
 * Derives and constructs [[HashOrd]] instances.
 */
object HashOrd {

  /**
   * Combines the implicit `Hash` and `Ord` instances into one [[HashOrd]]. It hashes with the `Hash` instance, and
   * compares and checks equality with the `Ord` instance.
   */
  implicit def derive[A](implicit hash0: Hash[A], ord0: Ord[A]): HashOrd[A] =
    new HashOrd[A] {
      def hash(a: A): Int                                    = hash0.hash(a)
      protected def checkCompare(l: A, r: A): Ordering       = ord0.compare(l, r)
      override protected def checkEqual(l: A, r: A): Boolean = ord0.equal(l, r)
    }

  /**
   * Constructs an instance from a `hash0` function, an `ord` function and an `equal0` function.
   * Equality checks call `equal0` instead of `ord`, so `equal0` can be cheaper than computing the ordering.
   */
  def make[A](hash0: A => Int, ord: (A, A) => Ordering, equal0: (A, A) => Boolean): Hash[A] with Ord[A] =
    new HashOrd[A] {
      def hash(a: A): Int                                       = hash0(a)
      override protected def checkCompare(l: A, r: A): Ordering = ord(l, r)
      override protected def checkEqual(l: A, r: A): Boolean    = equal0(l, r)
    }

  /**
   * Constructs an instance from a `hash0` function and an `ord` function.
   * Two values that are not the same reference are equal when `ord` returns `Ordering.Equals` for them.
   */
  def make[A](hash0: A => Int, ord: (A, A) => Ordering): Hash[A] with Ord[A] =
    new HashOrd[A] {
      def hash(a: A): Int                                       = hash0(a)
      override protected def checkCompare(l: A, r: A): Ordering = ord(l, r)
      override protected def checkEqual(l: A, r: A): Boolean    = ord(l, r).isEqual
    }

  /**
   * Constructs a `Hash[A] with Ord[A]` that hashes with `##` (which is `hashCode` except for numeric types, where it
   * agrees with `==`), checks equality with `==`, and orders with the implicit [[scala.math.Ordering]].
   */
  def default[A](implicit ord: scala.math.Ordering[A]): Hash[A] with Ord[A] =
    make(_.##, (l, r) => Ordering.fromCompare(ord.compare(l, r)), _ == _)

}
