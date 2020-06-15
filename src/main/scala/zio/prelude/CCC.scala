package zio.prelude

trait CCC[:=>[-_, +_]] extends IdentityCompose[:=>] {
  def duplicate[A]: A :=> (A, A)

  def both[A, B, C, D](f: A :=> B, g: C :=> D): (A, C) :=> (B, D)
  def first[A, B, C](f: A :=> (B, C)): A :=> B
  def second[A, B, C](f: A :=> (B, C)): A :=> C

  def swapBoth[A, B]: (A, B) :=> (B, A)

  def lassocBoth[A, B, C]: (A, (B, C)) :=> ((A, B), C)
  def rassocBoth[A, B, C]: ((A, B), C) :=> (A, (B, C))

  def either[A, B, C, D](f: A :=> B, g: C :=> D): Either[A, C] :=> Either[B, D]
  def left[A, B, C](f: Either[A, B] :=> C): A :=> C
  def right[A, B, C](f: Either[A, B] :=> C): B :=> C

  def swapEither[A, B]: Either[A, B] :=> Either[B, A]

  def lassocEither[A, B, C]: Either[A, Either[B, C]] :=> Either[Either[A, B], C]
  def rassocEither[A, B, C]: Either[Either[A, B], C] :=> Either[A, Either[B, C]]

  def apply[A, B](f: A :=> B, a: A): Any :=> B

  def curry[A, B, C](f: (A, B) :=> C): A :=> (B :=> C)
  def uncurry[A, B, C](f: A :=> (B :=> C)): (A, B) :=> C
}
