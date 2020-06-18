package zio.prelude

trait FunctionBoth[:=>[-_, +_]] extends IdentityCompose[:=>] {
  def both[A, B, C, D](f: A :=> B, g: C :=> D): (A, C) :=> (B, D)

  def toRightBoth[A]: (Unit, A) :=> A
  def toLeftBoth[A]: (A, Unit) :=> A

  def lassocBoth[A, B, C]: (A, (B, C)) :=> ((A, B), C)
  def rassocBoth[A, B, C]: ((A, B), C) :=> (A, (B, C))
}

trait FunctionSwapBoth[:=>[-_, +_]] extends FunctionBoth[:=>] {
  def swapBoth[A, B]: (A, B) :=> (B, A)
}

trait Cartesian[:=>[-_, +_]] extends FunctionSwapBoth[:=>] {
  def duplicateBoth[A]: A :=> (A, A)
  def delete[A]: A :=> Unit

  def leftDeleteLaw[A] = {
    compose(toLeftBoth[A], compose(both(identity[A], delete[A]), duplicateBoth[A])) == identity[A]
  }
  def rightDeleteLaw[A] = {
    compose(toRightBoth[A], compose(both(delete[A], identity[A]), duplicateBoth[A])) == identity[A]
  }
}

trait FunctionEither[:=>[-_, +_]] extends IdentityCompose[:=>] {
  def either[A, B, C, D](f: A :=> B, g: C :=> D): Either[A, C] :=> Either[B, D]
  
  def toRightEither[A]: Either[Nothing, A] :=> A
  def toLeftEither[A]: Either[A, Nothing] :=> A

  def lassocEither[A, B, C]: Either[A, Either[B, C]] :=> Either[Either[A, B], C]
  def rassocEither[A, B, C]: Either[Either[A, B], C] :=> Either[A, Either[B, C]]
}

trait FunctionSwapEither[:=>[-_, +_]] extends FunctionEither[:=>] {
  def swapEither[A, B]: Either[A, B] :=> Either[B, A]
}

trait Cocartesian[:=>[-_, +_]] extends FunctionSwapEither[:=>] {
  def duplicateEither[A]: A :=> Either[A, A]
  def fail[A]: A :=> Nothing

  def leftDeleteLaw[A] = {
    compose(toLeftEither[A], compose(either(identity[A], fail[A]), duplicateEither[A])) == identity[A]
  }
  def rightDeleteLaw[A] = {
    compose(toRightEither[A], compose(either(fail[A], identity[A]), duplicateEither[A])) == identity[A]
  }
}

trait CartesianClosed[:=>[-_, +_]] extends Cartesian[:=>] {
  def curry[A, B, C](f: (A, B) :=> C): A :=> (B :=> C)
  def uncurry[A, B, C](f: A :=> (B :=> C)): (A, B) :=> C
}

trait CartesianApply[:=>[-_, +_]] extends CartesianClosed[:=>] {
  def apply[A, B](f: A :=> B, a: A): Any :=> B
}
