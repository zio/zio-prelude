package zio.prelude

import scala.collection.BuildFrom

trait InvariantVersionSpecific {

  /**
   * Derives a `Traverable[F]` from an `Iterable[F]`.
   */
  implicit def IterableTraversable[F[+a] <: Iterable[a]](implicit derive: DeriveBuildFrom[F]): Traversable[F] =
    new Traversable[F] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        fa.foldLeft(derive.derive[B].newBuilder(fa).succeed)((bs, a) => bs.zipWith(f(a))(_ += _)).map(_.result())
    }

  trait DeriveBuildFrom[F[+_]] {
    def derive[A]: BuildFrom[F[Any], A, F[A]]
  }

  object DeriveBuildFrom {
    implicit def default[F[+_]](implicit bf: BuildFrom[F[Any], Any, F[Any]]): DeriveBuildFrom[F] =
      new DeriveBuildFrom[F] {
        def derive[A]: BuildFrom[F[Any], A, F[A]] =
          bf.asInstanceOf[BuildFrom[F[Any], A, F[A]]]
      }
  }
}
