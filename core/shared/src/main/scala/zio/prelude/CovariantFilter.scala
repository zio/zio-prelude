package zio.prelude

import zio.Chunk
import zio.prelude.coherent.CovariantIdentityBoth

trait CovariantFilter[F[+_]] {
  def covariant: Covariant[F]
  def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B]
  def collect[A, B](fa: F[A])(f: PartialFunction[A, B]): F[B] =
    mapFilter(fa)(f.lift)
}

object CovariantFilter {
  def apply[F[+_]](implicit covariantFilter: CovariantFilter[F]): CovariantFilter[F] =
    covariantFilter

  /**
   * The `ForEachFilter` (and thus `CovariantFilter` that has a `Covariant`) for `Chunk`.
   */
  implicit def ChunkForEachFilter(implicit ForEach: ForEach[Chunk]): ForEachFilter[Chunk] =
    new ForEachFilter[Chunk] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](fa: Chunk[A])(f: A => G[Option[B]]): G[Chunk[B]] =
        CovariantIdentityBoth[G].forEachFilter(fa)(f)

      def forEach: ForEach[Chunk] = ForEach
    }

  /**
   * The `ForEachFilter` instance for `Const`.
   */
  implicit def ConstForEachFilter[A]: ForEachFilter[({ type ConstA[+B] = Const[A, B] })#ConstA] =
    new ForEachFilter[({ type ConstA[+B] = Const[A, B] })#ConstA] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, B, C](
        fa: Const[A, B]
      )(f: B => G[Option[C]]): G[Const[A, C]] =
        Const.wrap(Const.unwrap(fa)).succeed

      def forEach: ForEach[Const[A, +*]] = Invariant.ConstForEach[A]
    }

  /**
   * The `ForEachFilter` (and thus `CovariantFilter` that has a `Covariant`) instance for `List`.
   */
  implicit def ListForEachFilter(implicit ForEach: ForEach[List]): ForEachFilter[List] =
    new ForEachFilter[List] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](fa: List[A])(f: A => G[Option[B]]): G[List[B]] =
        CovariantIdentityBoth[G].forEachFilter(fa)(f)

      def forEach: ForEach[List] = ForEach
    }

  /**
   * The `ForEachFilter` (and thus `CovariantFilter` that has a `Covariant`) instance for `Map`.
   */
  implicit def MapForEachFilter[K](implicit
    ForEach: ForEach[({ type lambda[+v] = Map[K, v] })#lambda]
  ): ForEachFilter[({ type lambda[+v] = Map[K, v] })#lambda] =
    new ForEachFilter[({ type lambda[+v] = Map[K, v] })#lambda] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, V, V2](map: Map[K, V])(f: V => G[Option[V2]]): G[Map[K, V2]] =
        CovariantIdentityBoth[G]
          .forEachFilter[(K, V), (K, V2), Iterable](map) { case (k, v) => f(v).map(_.map(k -> _)) }
          .map(_.toMap)

      def forEach: ForEach[({ type lambda[+v] = Map[K, v] })#lambda] = ForEach
    }

  /**
   * The `ForEachFilter` (and thus `CovariantFilter` that has a `Covariant`) instance for `Option`.
   */
  implicit def OptionForEachFilter(implicit ForEach: ForEach[Option]): ForEachFilter[Option] =
    new ForEachFilter[Option] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](option: Option[A])(f: A => G[Option[B]]): G[Option[B]] =
        option.fold[G[Option[B]]](Option.empty.succeed)(f)

      def forEach: ForEach[Option] = ForEach
    }

  /**
   * The `ForEachFilter` (and thus `CovariantFilter` that has a `Covariant`) instance for `Vector`.
   */
  implicit def VectorForEachFilter(implicit ForEach: ForEach[Vector]): ForEachFilter[Vector] =
    new ForEachFilter[Vector] {
      def forEachFilter[G[+_]: IdentityBoth: Covariant, A, B](
        as: Vector[A]
      )(f: A => G[Option[B]]): G[Vector[B]] =
        CovariantIdentityBoth[G].forEachFilter(as)(f)

      def forEach: ForEach[Vector] = ForEach
    }
}

trait CovariantFilterSyntax {
  implicit class CovariantFilterOps[F[+_], A](private val self: F[A]) {
    def mapFilter[B](f: A => Option[B])(implicit C: CovariantFilter[F]): F[B] =
      C.mapFilter(self)(f)

    def collect[B](f: PartialFunction[A, B])(implicit C: CovariantFilter[F]): F[B] =
      C.collect(self)(f)
  }
}
