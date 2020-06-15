package zio.prelude

trait Strong[:=>[-_, +_]] extends Divariant[:=>] {
  def first[A, B, C](f: A :=> B): (A, C) :=> (B, C)

  def second[A, B, C](f: A :=> B): (C, A) :=> (C, B)
}

trait Costrong[:=>[-_, +_]] extends Divariant[:=>] {
  def unfirst[A, B, C](f: (A, C) :=> (B, C)): A :=> B

  def unsecond[A, B, C](f: (C, A) :=> (C, B)): A :=> B
}
