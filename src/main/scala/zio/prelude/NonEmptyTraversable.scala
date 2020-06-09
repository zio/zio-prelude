package zio.prelude

trait NonEmptyTraversable[F[+_]] extends Traversable[F] {
  def foldMap1[A, B: Closure](fa: F[A])(f: A => B): B = ???

  def mapEffect1[G[+_]: AssociativeBoth: Covariant, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def reduceMap1[A: Closure](fa: F[A])(f: (A, A) => A): A = ???

  // mapEffect(fa)(a => Id(a)) === Id(fa)
}
