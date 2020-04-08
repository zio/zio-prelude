package zio.prelude

trait Invariant[F[_]] {
  def invariantMap[A, B](f: A <=> B): F[A] <=> F[B]
}
object Invariant {}
