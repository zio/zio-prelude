package zio.prelude

import zio.NonEmptyChunk

trait NonEmptyTraversable[F[+_]] extends Traversable[F] {
  def flip1[G[+_]: AssociativeBoth: Covariant, A, B](fa: F[G[A]]): G[F[A]] =
    foreach1(fa)(identity(_))

  def foldMap1[A, B: Closure](fa: F[A])(f: A => B): B =
    reduceMap1(map(f)(fa))

  def foreach[G[+_]: AssociativeBoth: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    foreach1(fa)(f)

  def foreach1[G[+_]: AssociativeBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def foreach1_[G[+_]: AssociativeBoth: Covariant, A](fa: F[A])(f: A => G[Any]): G[Unit] =
    foreach1(fa)(f).as(())

  def reduce[A](fa: F[A])(f: (A, A) => A): A = {
    implicit val closure: Closure[A] = f(_, _)

    reduceMap1(fa)
  }

  def reduceMap1[A: Closure](fa: F[A]): A = {
    import NonEmptyTraversable.internal._

    type G[+B] = Collector[A, B]

    foreach1[G, A, A](fa)(Collector(_)).run(_ combine _)
  }

  def toNonEmptyChunk[A](fa: F[A]): NonEmptyChunk[A] =
    reduceMap1(map((a: A) => NonEmptyChunk[A](a))(fa))
}
object NonEmptyTraversable {
  private[NonEmptyTraversable] object internal {
    final case class Collector[A, +B](run: ((A, A) => A) => A) { self =>
      def map[C](f: B => C): Collector[A, C] = {
        val _ = f
        Collector(run)
      }

      def zip[C](that: Collector[A, C]): Collector[A, (B, C)] =
        Collector[A, (B, C)](combine => combine(self.run(combine), that.run(combine)))
    }
    object Collector {
      def apply[A](a: A): Collector[A, Nothing] =
        Collector(_ => a)

      implicit def associativeBothCollector[A]: AssociativeBoth[({ type lambda[+B] = Collector[A, B] })#lambda]
        with Covariant[({ type lambda[+B]                                          = Collector[A, B] })#lambda] =
        new AssociativeBoth[({ type lambda[+B] = Collector[A, B] })#lambda]
          with Covariant[({ type lambda[+B]    = Collector[A, B] })#lambda] {
          def both[X, Y](fa: => Collector[A, X], fb: => Collector[A, Y]): Collector[A, (X, Y)] =
            fa zip fb

          def map[X, Y](f: X => Y): Collector[A, X] => Collector[A, Y] = fa => fa.map(f)
        }
    }
  }
}
