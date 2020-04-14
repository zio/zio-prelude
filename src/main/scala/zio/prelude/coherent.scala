package zio.prelude

package coherent {

  trait AssociativeCommutativeEqual[A] extends AssociativeEqual[A] with Commutative[A]

  object AssociativeCommutativeEqual {
    implicit def derive[A](
      implicit associative0: Associative[A],
      commutative0: Commutative[A],
      equal0: Equal[A]
    ): AssociativeCommutativeEqual[A] =
      new AssociativeCommutativeEqual[A] {
        val _                                         = associative0
        def combine(l: => A, r: => A): A              = commutative0.combine(l, r)
        protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
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

  trait AssociativeIdentity[A] extends Associative[A] with Identity[A]

  object AssociativeIdentity {
    implicit def derive[A](implicit associative0: Associative[A], identity0: Identity[A]): AssociativeIdentity[A] =
      new AssociativeIdentity[A] {
        def combine(l: => A, r: => A): A = associative0.combine(l, r)
        def identity: A                  = identity0.identity
      }
  }

  trait ClosureEqual[A] extends Closure[A] with Equal[A]

  object ClosureEqual {
    implicit def derive[A](implicit closure0: Closure[A], equal0: Equal[A]): ClosureEqual[A] =
      new ClosureEqual[A] {
        def combine(l: => A, r: => A): A              = closure0.combine(l, r)
        protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
      }
  }

  trait CommutativeEqual[A] extends ClosureEqual[A] with Commutative[A]

  object CommutativeEqual {
    implicit def derive[A](implicit commutative0: Commutative[A], equal0: Equal[A]): CommutativeEqual[A] =
      new CommutativeEqual[A] {
        def combine(l: => A, r: => A): A              = commutative0.combine(l, r)
        protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
      }
  }

  trait CovariantEqualF[F[+_]] extends Covariant[F] with EqualF[F]

  object CovariantEqualF {
    implicit def derive[F[+_]](implicit covariant0: Covariant[F], equalF0: EqualF[F]): CovariantEqualF[F] =
      new CovariantEqualF[F] {
        def deriveEqual[A: Equal]: Equal[F[A]] =
          equalF0.deriveEqual
        def map[A, B](f: A => B): F[A] => F[B] =
          covariant0.map(f)
      }
  }

  trait HashOrd[-A] extends Hash[A] with Ord[A] { self =>
    final override def contramap[B](f: B => A): HashOrd[B] =
      HashOrd.derive(self.contramap(f), self.contramap(f))
  }

  object HashOrd {
    implicit def derive[A](implicit hash0: Hash[A], ord0: Ord[A]): HashOrd[A] =
      new HashOrd[A] {
        def hash(a: A): Int                                    = hash0.hash(a)
        protected def checkCompare(l: A, r: A): Ordering       = ord0.compare(l, r)
        override protected def checkEqual(l: A, r: A): Boolean = ord0.equal(l, r)
      }
  }

  trait EqualIdentity[A] extends ClosureEqual[A] with Identity[A]

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
        def inverse(a: A): A                                   = inverse0.inverse(a)
        override protected def checkEqual(l: A, r: A): Boolean = equal0.equal(l, r)
      }
  }
}
