package zio.prelude

import scala.collection.BuildFrom

trait InvariantVersionSpecific {

  /**
   * Derives a `ForEach[F]` from an `Iterable[F]`.
   */
  implicit def IterableForEach[F[+a] <: Iterable[a]](implicit derive: DeriveBuildFrom[F]): ForEach[F] =
    new ForEach[F] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
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
