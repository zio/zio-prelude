package zio.prelude

trait Choice[:=>[-_, +_]] extends Divariant[:=>] {
  def left[A, B, C](f: A :=> B): Either[A, C] :=> Either[B, C]

  def right[A, B, C](f: A :=> B): Either[C, A] :=> Either[C, B]
}

trait Cochoice[:=>[-_, +_]] extends Divariant[:=>] {
  def unleft[A, B, C](f: Either[A, C] :=> Either[B, C]): A :=> B

  def unright[A, B, C](f: Either[C, A] :=> Either[C, B]): A :=> B
}
