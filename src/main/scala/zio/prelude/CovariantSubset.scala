package zio.prelude

trait CovariantSubset[F[+_], TypeClass[_]] {
  def map[A, B: TypeClass](f: A => B): F[A] => F[B]
}
