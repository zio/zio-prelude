package zio.prelude

import zio.Chunk

trait Traversable[F[+_]] extends Covariant[F] {
  def fold[S, A](fa: F[A])(s: S)(f: (S, A) => S): S = {
    import Traversable.State

    type StateS[+A] = State[S, A]

    mapEffect[StateS, A, Any](fa)((a: A) => State((s: S) => (f(s, a), ()))).run(s)._1
  }

  def foldMap[A, B: Identity](fa: F[A])(f: A => B): B =
    fold[B, A](fa)(Identity[B].identity)((b: B, a: A) => b combine f(a))

  def flip[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](fa: F[G[A]]): G[F[A]] =
    mapEffect(fa)(identity(_))

  def foreach_[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
    mapEffect(fa)(f).as(())

  def map[A, B](f: A => B): F[A] => F[B] =
    (fa: F[A]) => Id.unwrap(mapEffect[Id, A, B](fa)((a: A) => Id(f(a))))

  def mapEffect[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def reduceMap[A: Identity](fa: F[A])(f: (A, A) => A): A =
    fold[A, A](fa)(Identity[A].identity)(f)

  def toChunk[A](fa: F[A]): Chunk[A] = foldMap(fa)(Chunk(_))

  // mapEffect(fa)(a => Id(a)) === Id(fa)
}
object Traversable {
  final case class State[S, +A](run: S => (S, A))
  object State {
    implicit def StateAssociativeBoth[S]: AssociativeBoth[({ type lambda[+A] = State[S, A] })#lambda] = {
      type StateS[+A] = State[S, A]

      new AssociativeBoth[StateS] {
        def both[A, B](fa: => StateS[A], fb: => StateS[B]): StateS[(A, B)] =
          State { (s0: S) =>
            val (s1, a) = fa.run(s0)
            val (s2, b) = fb.run(s1)

            (s2, (a, b))
          }
      }
    }
    implicit def StateIdentityBoth[S]: IdentityBoth[({ type lambda[+A] = State[S, A] })#lambda] = {
      type StateS[+A] = State[S, A]

      new IdentityBoth[StateS] {
        def any: StateS[Any] = State(s => (s, ()))

        def both[A, B](fa: => StateS[A], fb: => StateS[B]): StateS[(A, B)] =
          State { (s0: S) =>
            val (s1, a) = fa.run(s0)
            val (s2, b) = fb.run(s1)

            (s2, (a, b))
          }
      }
    }
    implicit def StateCovariant[S]: Covariant[({ type lambda[+A] = State[S, A] })#lambda] = {
      type StateS[+A] = State[S, A]

      new Covariant[StateS] {
        def map[A, B](f: A => B): StateS[A] => StateS[B] =
          (fa: StateS[A]) => State((s: S) => fa.run(s) match { case (s, a) => (s, f(a)) })
      }
    }
  }

  implicit val ListTraversable: Traversable[List] =
    new Traversable[List] {
      override def map[A, B](f: A => B): List[A] => List[B] =
        (list: List[A]) => list.map(f)

      def mapEffect[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](list: List[A])(f: A => G[B]): G[List[B]] =
        list match {
          case Nil     => Nil.succeed[G]
          case a :: as => (f(a) zipWith mapEffect(as)(f))(_ :: _)
        }
    }
}
