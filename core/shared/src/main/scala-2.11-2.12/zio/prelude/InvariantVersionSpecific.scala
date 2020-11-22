package zio.prelude

import scala.collection.generic.CanBuildFrom

trait InvariantVersionSpecific {

  /**
   * Derives a `Traverable[F]` from an `Iterable[F]`.
   */
  implicit def IterableTraversable[F[+a] <: Iterable[a]](implicit derive: DeriveCanBuildFrom[F]): Traversable[F] =
    new Traversable[F] {
      def foreach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        fa.foldLeft(derive.derive[B](fa).succeed)((bs, a) => bs.zipWith(f(a))(_ += _)).map(_.result())
    }

  trait DeriveCanBuildFrom[F[+_]] {
    def derive[A]: CanBuildFrom[F[Any], A, F[A]]
  }

  object DeriveCanBuildFrom {
    implicit def default[F[+_]](implicit bf: CanBuildFrom[F[Any], Any, F[Any]]): DeriveCanBuildFrom[F] =
      new DeriveCanBuildFrom[F] {
        def derive[A]: CanBuildFrom[F[Any], A, F[A]] =
          bf.asInstanceOf[CanBuildFrom[F[Any], A, F[A]]]
      }
  }
}
