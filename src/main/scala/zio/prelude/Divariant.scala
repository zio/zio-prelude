package zio.prelude

trait Divariant[:=>[-_, +_]] {
  def deriveCovariant[A]: Covariant[({ type lambda[+B] = A :=> B })#lambda] =
    ???

  def deriveContravariant[B]: Contravariant[({ type lambda[-A] = A :=> B })#lambda] =
    ???

  def dimap[A, B, C, D](f: C => A, g: B => D): (A :=> B) => (C :=> D) =
    (ab: A :=> B) => rightMap(g)(leftMap(f)(ab))

  def leftMap[A, B, C](f: C => A): (A :=> B) => (C :=> B)

  def rightMap[A, B, C](f: B => C): (A :=> B) => (A :=> C)
}
object Divariant {
  final case class Join[:=>[-_, +_], A](value: A :=> A)
}
