package zio.prelude

trait ForEachFilter[F[+_]] extends CovariantFilter[F] {
  def forEach: ForEach[F]
  final def covariant: Covariant[F] = forEach
  def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](fa: F[A])(f: A => G[Option[B]]): G[F[B]]

  def filterA[G[+_]: IdentityBoth: Covariant, A](fa: F[A])(f: A => G[Boolean]): G[F[A]] =
    forEachFilter(fa)(a => f(a).map(if (_) Some(a) else None))
  def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B]                                =
    Id.unwrap(forEachFilter[Id, A, B](fa)(f.andThen(Id(_))))
}

object ForEachFilter {

  /**
   * Summons an implicit `ForEachFilter[F]`.
   */
  def apply[F[+_]](implicit forEachFilter: ForEachFilter[F]): ForEachFilter[F] =
    forEachFilter
}

trait ForEachFilterSyntax {
  implicit class ForEachFilterOps[F[+_], A](private val self: F[A]) {
    def forEachFilter[G[+_]: IdentityBoth: Covariant, B](f: A => G[Option[B]])(implicit
      forEachFilter: ForEachFilter[F]
    ): G[F[B]] =
      forEachFilter.forEachFilter(self)(f)

    def filterA[G[+_]: IdentityBoth: Covariant](f: A => G[Boolean])(implicit
      forEachFilter: ForEachFilter[F]
    ): G[F[A]] =
      forEachFilter.filterA(self)(f)
  }
}
