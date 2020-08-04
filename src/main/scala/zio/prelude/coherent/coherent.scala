package zio.prelude.coherent

import zio.prelude._

trait AssociativeBothDeriveEqualInvariant[F[_]] extends AssociativeBoth[F] with DeriveEqual[F] with Invariant[F]

object AssociativeBothDeriveEqualInvariant {
  implicit def derive[F[_]](
    implicit associativeBoth0: AssociativeBoth[F],
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
  implicit def derive[F[_]](
    implicit associativeEither0: AssociativeEither[F],
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
  implicit def derive[F[+_]](
    implicit associativeFlatten0: AssociativeFlatten[F],
    covariant0: Covariant[F],
    deriveEqual0: DeriveEqual[F]
  ): AssociativeFlattenCovariantDeriveEqual[F] =
    new AssociativeFlattenCovariantDeriveEqual[F] {
      def derive[A: Equal]: Equal[F[A]] =
        deriveEqual0.derive
      def flatten[A](ffa: F[F[A]]): F[A] =
        associativeFlatten0.flatten(ffa)
      def map[A, B](f: A => B): F[A] => F[B] =
        covariant0.map(f)
    }
}

trait AssociativeIdentity[A] extends Associative[A] with Identity[A]

object AssociativeIdentity {
  implicit def derive[A](implicit associative0: Associative[A], identity0: Identity[A]): AssociativeIdentity[A] =
    new AssociativeIdentity[A] {
      def combine(l: => A, r: => A): A = associative0.combine(l, r)
      def identity: A                  = identity0.identity
    }
}

trait CommutativeBothDeriveEqualInvariant[F[_]] extends CommutativeBoth[F] with DeriveEqual[F] with Invariant[F]

object CommutativeBothDeriveEqualInvariant {
  implicit def derive[F[_]](
    implicit commutativeBoth0: CommutativeBoth[F],
    deriveEqual0: DeriveEqual[F],
    invariant0: Invariant[F]
  ): CommutativeBothDeriveEqualInvariant[F] =
    new CommutativeBothDeriveEqualInvariant[F] {
      def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] = commutativeBoth0.both(fa, fb)
      def derive[A: Equal]: Equal[F[A]]                   = deriveEqual0.derive
      def invmap[A, B](f: A <=> B): F[A] <=> F[B]         = invariant0.invmap(f)
    }
}

trait CommutativeEitherDeriveEqualInvariant[F[_]] extends CommutativeEither[F] with DeriveEqual[F] with Invariant[F]

object CommutativeEitherDeriveEqualInvariant {
  implicit def derive[F[_]](
    implicit commutativeEither0: CommutativeEither[F],
    deriveEqual0: DeriveEqual[F],
    invariant0: Invariant[F]
  ): CommutativeEitherDeriveEqualInvariant[F] =
    new CommutativeEitherDeriveEqualInvariant[F] {
      def derive[A: Equal]: Equal[F[A]]                           = deriveEqual0.derive
      def either[A, B](fa: => F[A], fb: => F[B]): F[Either[A, B]] = commutativeEither0.either(fa, fb)
      def invmap[A, B](f: A <=> B): F[A] <=> F[B]                 = invariant0.invmap(f)
    }
}

trait CommutativeEqual[A] extends Commutative[A] with Equal[A]

object CommutativeEqual {
  implicit def derive[A](implicit commutative0: Commutative[A], equal0: Equal[A]): CommutativeEqual[A] =
    new CommutativeEqual[A] {
      def combine(l: => A, r: => A): A              = commutative0.combine(l, r)
      protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
    }
}

trait CovariantDeriveEqual[F[+_]] extends Covariant[F] with DeriveEqual[F]

object CovariantDeriveEqual {
  implicit def derive[F[+_]](implicit covariant0: Covariant[F], deriveEqual0: DeriveEqual[F]): CovariantDeriveEqual[F] =
    new CovariantDeriveEqual[F] {
      def derive[A: Equal]: Equal[F[A]] =
        deriveEqual0.derive
      def map[A, B](f: A => B): F[A] => F[B] =
        covariant0.map(f)
    }
}

trait ContravariantDeriveEqual[F[-_]] extends Contravariant[F] with DeriveEqual[F]

object ContravariantDeriveEqual {
  implicit def derive[F[-_]](
    implicit contravariant0: Contravariant[F],
    deriveEqual0: DeriveEqual[F]
  ): ContravariantDeriveEqual[F] =
    new ContravariantDeriveEqual[F] {
      def derive[A: Equal]: Equal[F[A]] =
        deriveEqual0.derive
      def contramap[A, B](f: B => A): F[A] => F[B] =
        contravariant0.contramap(f)
    }
}

trait CovariantDeriveEqualIdentityFlatten[F[+_]]
    extends IdentityFlatten[F]
    with AssociativeFlattenCovariantDeriveEqual[F]

object CovariantDeriveEqualIdentityFlatten {
  implicit def derive[F[+_]](
    implicit covariant0: Covariant[F],
    deriveEqual0: DeriveEqual[F],
    identityFlatten0: IdentityFlatten[F]
  ): CovariantDeriveEqualIdentityFlatten[F] =
    new CovariantDeriveEqualIdentityFlatten[F] {
      def any: F[Any] =
        identityFlatten0.any
      def derive[A: Equal]: Equal[F[A]] =
        deriveEqual0.derive
      def flatten[A](ffa: F[F[A]]): F[A] =
        identityFlatten0.flatten(ffa)
      def map[A, B](f: A => B): F[A] => F[B] =
        covariant0.map(f)
    }
}

trait DeriveEqualIdentityBothInvariant[F[_]] extends DeriveEqual[F] with IdentityBoth[F] with Invariant[F]

object DeriveEqualIdentityBothInvariant {
  implicit def derive[F[_]](
    implicit deriveEqual0: DeriveEqual[F],
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

trait DeriveEqualIdentityEitherInvariant[F[_]] extends DeriveEqual[F] with IdentityEither[F] with Invariant[F]

object DeriveEqualIdentityEitherInvariant {
  implicit def derive[F[_]](
    implicit deriveEqual0: DeriveEqual[F],
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

trait DeriveEqualNonEmptyTraversable[F[+_]] extends DeriveEqualTraversable[F] with NonEmptyTraversable[F]

object DeriveEqualNonEmptyTraversable {
  implicit def derive[F[+_]](
    implicit deriveEqual0: DeriveEqual[F],
    nonEmptyTraversable0: NonEmptyTraversable[F]
  ): DeriveEqualNonEmptyTraversable[F] =
    new DeriveEqualNonEmptyTraversable[F] {
      def derive[A: Equal]: Equal[F[A]] =
        deriveEqual0.derive
      def foreach1[G[+_]: AssociativeBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        nonEmptyTraversable0.foreach1(fa)(f)
    }
}

trait DeriveEqualTraversable[F[+_]] extends CovariantDeriveEqual[F] with Traversable[F]

object DeriveEqualTraversable {
  implicit def derive[F[+_]](
    implicit deriveEqual0: DeriveEqual[F],
    traversable0: Traversable[F]
  ): DeriveEqualTraversable[F] =
    new DeriveEqualTraversable[F] {
      def derive[A: Equal]: Equal[F[A]] =
        deriveEqual0.derive
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        traversable0.foreach(fa)(f)
    }
}

trait EqualIdentity[A] extends Equal[A] with Identity[A]

object EqualIdentity {
  implicit def derive[A](implicit identity0: Identity[A], equal0: Equal[A]): EqualIdentity[A] =
    new EqualIdentity[A] {
      def combine(l: => A, r: => A): A              = identity0.combine(l, r)
      def identity: A                               = identity0.identity
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

trait HashOrd[-A] extends Hash[A] with Ord[A] { self =>
  final override def contramap[B](f: B => A): HashOrd[B] =
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
}
