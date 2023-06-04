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

trait AssociativeBothDeriveEqualInvariant[F[_]] extends AssociativeBoth[F] with DeriveEqual[F] with Invariant[F]

object AssociativeBothDeriveEqualInvariant {
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

trait AssociativeEitherDeriveEqualInvariant[F[_]] extends AssociativeEither[F] with DeriveEqual[F] with Invariant[F]

object AssociativeEitherDeriveEqualInvariant {
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

trait AssociativeEqual[A] extends Associative[A] with Equal[A]

object AssociativeEqual {
  implicit def derive[A](implicit associative0: Associative[A], equal0: Equal[A]): AssociativeEqual[A] =
    new AssociativeEqual[A] {
      def combine(l: => A, r: => A): A              = associative0.combine(l, r)
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait AssociativeFlattenCovariantDeriveEqual[F[+_]] extends AssociativeFlatten[F] with Covariant[F] with DeriveEqual[F]

object AssociativeFlattenCovariantDeriveEqual {
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

trait CommutativeBothDeriveEqualInvariant[F[_]] extends AssociativeBothDeriveEqualInvariant[F] with CommutativeBoth[F]

object CommutativeBothDeriveEqualInvariant {
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

trait CommutativeEitherDeriveEqualInvariant[F[_]]
    extends AssociativeEitherDeriveEqualInvariant[F]
    with CommutativeEither[F]

object CommutativeEitherDeriveEqualInvariant {
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

trait CommutativeEqual[A] extends AssociativeEqual[A] with Commutative[A]

object CommutativeEqual {
  implicit def derive[A](implicit commutative0: Commutative[A], equal0: Equal[A]): CommutativeEqual[A] =
    new CommutativeEqual[A] {
      def combine(l: => A, r: => A): A              = commutative0.combine(l, r)
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait CommutativeIdentity[A] extends Commutative[A] with Identity[A]

object CommutativeIdentity {
  implicit def derive[A](implicit commutative0: Commutative[A], identity0: Identity[A]): CommutativeIdentity[A] =
    new CommutativeIdentity[A] {
      def combine(l: => A, r: => A): A = commutative0.combine(l, r)
      def identity: A                  = identity0.identity
    }
}

trait CovariantDeriveEqual[F[+_]] extends Covariant[F] with DeriveEqual[F]

object CovariantDeriveEqual {
  implicit def derive[F[+_]](implicit covariant0: Covariant[F], deriveEqual0: DeriveEqual[F]): CovariantDeriveEqual[F] =
    new CovariantDeriveEqual[F] {
      def derive[A: Equal]: Equal[F[A]]      =
        deriveEqual0.derive
      def map[A, B](f: A => B): F[A] => F[B] =
        covariant0.map(f)
    }
}

trait ContravariantDeriveEqual[F[-_]] extends Contravariant[F] with DeriveEqual[F]

object ContravariantDeriveEqual {
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

trait CovariantDeriveEqualIdentityFlatten[F[+_]]
    extends IdentityFlatten[F]
    with AssociativeFlattenCovariantDeriveEqual[F]

object CovariantDeriveEqualIdentityFlatten {
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

trait CovariantIdentityBoth[F[+_]] extends Covariant[F] with IdentityBoth[F] { self =>
  private implicit val covariant: Covariant[F]       = self
  private implicit val identityBoth: IdentityBoth[F] = self

  def forEach[A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(f: A => F[B])(implicit
    bf: BuildFrom[Collection[A], B, Collection[B]]
  ): F[Collection[B]] =
    in.foldLeft(bf.newBuilder(in).succeed)((bs, a) => bs.zipWith(f(a))(_ += _)).map(_.result())

  def forEach_[A, B](in: Iterable[A])(f: A => F[Any]): F[Unit] =
    in.foldLeft(identityBoth.any)((bs, a) => bs *> f(a)).unit

  def forEachFilter[A, B, Collection[+Element] <: Iterable[Element]](in: Collection[A])(f: A => F[Option[B]])(implicit
    bf: BuildFrom[Collection[A], B, Collection[B]]
  ): F[Collection[B]] =
    in.foldLeft(bf.newBuilder(in).succeed)((bs, a) => bs.zipWith(f(a))(_ ++= _)).map(_.result())
}

object CovariantIdentityBoth {

  def apply[F[+_]](implicit covariantIdentityBoth: CovariantIdentityBoth[F]): CovariantIdentityBoth[F] =
    covariantIdentityBoth

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

trait DeriveEqualIdentityBothInvariant[F[_]] extends AssociativeBothDeriveEqualInvariant[F] with IdentityBoth[F]

object DeriveEqualIdentityBothInvariant {
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

trait DeriveEqualIdentityEitherInvariant[F[_]] extends AssociativeEitherDeriveEqualInvariant[F] with IdentityEither[F]

object DeriveEqualIdentityEitherInvariant {
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

trait DeriveEqualNonEmptyForEach[F[+_]] extends DeriveEqualForEach[F] with NonEmptyForEach[F]

object DeriveEqualNonEmptyForEach {
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

trait DeriveEqualForEach[F[+_]] extends CovariantDeriveEqual[F] with ForEach[F]

object DeriveEqualForEach {
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

trait CovariantFilterDeriveEqual[F[+_]] extends DeriveEqual[F] with CovariantFilter[F]

object CovariantFilterDeriveEqual {
  implicit def derive[F[+_]](implicit
    deriveEqual0: DeriveEqual[F],
    covariantFilter0: CovariantFilter[F]
  ): CovariantFilterDeriveEqual[F] =
    new CovariantFilterDeriveEqual[F] {
      def derive[A: Equal]: Equal[F[A]]                      = deriveEqual0.derive
      def covariant: Covariant[F]                            = covariantFilter0.covariant
      def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B] = covariantFilter0.mapFilter(fa)(f)
    }
}

trait DeriveEqualForEachFilter[F[+_]] extends CovariantFilterDeriveEqual[F] with ForEachFilter[F]

object DeriveEqualForEachFilter {
  implicit def derive[F[+_]](implicit
    deriveEqual0: DeriveEqual[F],
    forEachFilter0: ForEachFilter[F]
  ): DeriveEqualForEachFilter[F] =
    new DeriveEqualForEachFilter[F] {
      def derive[A: Equal]: Equal[F[A]]                                                                = deriveEqual0.derive
      def forEach: ForEach[F]                                                                          = forEachFilter0.forEach
      def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[Option[B]]): G[F[B]] =
        forEachFilter0.forEachFilter(fa)(f)
    }
}

trait EqualIdentity[A] extends AssociativeEqual[A] with Identity[A]

object EqualIdentity {
  implicit def derive[A](implicit identity0: Identity[A], equal0: Equal[A]): EqualIdentity[A] =
    new EqualIdentity[A] {
      def combine(l: => A, r: => A): A              = identity0.combine(l, r)
      def identity: A                               = identity0.identity
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait EqualIdempotent[A] extends AssociativeEqual[A] with Idempotent[A]

object EqualIdempotent {
  implicit def derive[A](implicit idempotent0: Idempotent[A], equal0: Equal[A]): EqualIdempotent[A] =
    new EqualIdempotent[A] {
      def combine(l: => A, r: => A): A              = idempotent0.combine(l, r)
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait EqualInverse[A] extends EqualIdentity[A] with Inverse[A]

object EqualInverse {
  implicit def derive[A](implicit equal0: Equal[A], inverse0: Inverse[A]): EqualInverse[A] =
    new EqualInverse[A] {
      def combine(l: => A, r: => A): A                       = inverse0.combine(l, r)
      def identity: A                                        = inverse0.identity
      def inverse(l: => A, r: => A): A                       = inverse0.inverse(l, r)
      override protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait HashPartialOrd[-A] extends Hash[A] with PartialOrd[A] { self =>
  override def contramap[B](f: B => A): Hash[B] with PartialOrd[B] =
    new HashPartialOrd[B] {
      def hash(b: B): Int                                     = self.hash(f(b))
      protected def checkCompare(l: B, r: B): PartialOrdering = self.compare(f(l), f(r))
      override protected def checkEqual(l: B, r: B): Boolean  = self.equal(f(l), f(r))
    }
}

object HashPartialOrd {
  implicit def derive[A](implicit hash0: Hash[A], ord0: PartialOrd[A]): HashPartialOrd[A] =
    new HashPartialOrd[A] {
      def hash(a: A): Int                                     = hash0.hash(a)
      protected def checkCompare(l: A, r: A): PartialOrdering = ord0.compare(l, r)
      override protected def checkEqual(l: A, r: A): Boolean  = ord0.equal(l, r)
    }

  /**
   * Constructs an instance from a `hash0` function, an `ord`` function and a `equal0` function.
   * Since this takes a separate `equal0`, short-circuiting the equality check (failing fast) is possible.
   */
  def make[A](hash0: A => Int, ord: (A, A) => PartialOrdering, equal0: (A, A) => Boolean): Hash[A] with PartialOrd[A] =
    new HashPartialOrd[A] {
      def hash(a: A): Int                                              = hash0(a)
      override protected def checkCompare(l: A, r: A): PartialOrdering = ord(l, r)
      override protected def checkEqual(l: A, r: A): Boolean           = equal0(l, r)
    }

  /**
   * Constructs an instance from a hash function, equal function and ord function.
   * Checking equality is delegated to `ord`, so short-circuiting the equality check (failing fast) is not possible.
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

trait HashOrd[-A] extends HashPartialOrd[A] with Ord[A] { self =>
  final override def contramap[B](f: B => A): Hash[B] with Ord[B] =
    new HashOrd[B] {
      def hash(b: B): Int                                    = self.hash(f(b))
      protected def checkCompare(l: B, r: B): Ordering       = self.compare(f(l), f(r))
      override protected def checkEqual(l: B, r: B): Boolean = self.equal(f(l), f(r))
    }
}

object HashOrd {
  implicit def derive[A](implicit hash0: Hash[A], ord0: Ord[A]): HashOrd[A] =
    new HashOrd[A] {
      def hash(a: A): Int                                    = hash0.hash(a)
      protected def checkCompare(l: A, r: A): Ordering       = ord0.compare(l, r)
      override protected def checkEqual(l: A, r: A): Boolean = ord0.equal(l, r)
    }

  /**
   * Constructs an instance from a `hash0` function, an `ord`` function and a `equal0` function.
   * Since this takes a separate `equal0`, short-circuiting the equality check (failing fast) is possible.
   */
  def make[A](hash0: A => Int, ord: (A, A) => Ordering, equal0: (A, A) => Boolean): Hash[A] with Ord[A] =
    new HashOrd[A] {
      def hash(a: A): Int                                       = hash0(a)
      override protected def checkCompare(l: A, r: A): Ordering = ord(l, r)
      override protected def checkEqual(l: A, r: A): Boolean    = equal0(l, r)
    }

  /**
   * Constructs an instance from a hash function, equal function and ord function.
   * Checking equality is delegated to `ord`, so short-circuiting the equality check (failing fast) is not possible.
   */
  def make[A](hash0: A => Int, ord: (A, A) => Ordering): Hash[A] with Ord[A] =
    new HashOrd[A] {
      def hash(a: A): Int                                       = hash0(a)
      override protected def checkCompare(l: A, r: A): Ordering = ord(l, r)
      override protected def checkEqual(l: A, r: A): Boolean    = ord(l, r).isEqual
    }

  /**
   * Constructs a `Hash[A] with Ord[A]` that uses the default notion of hashing embodied in
   * the implementation of `hashCode` for values of type `A` and ordering from [[scala.math.Ordering]].
   */
  def default[A](implicit ord: scala.math.Ordering[A]): Hash[A] with Ord[A] =
    make(_.##, (l, r) => Ordering.fromCompare(ord.compare(l, r)), _ == _)

}
