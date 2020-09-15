package zio.prelude

trait RightCovariant[:=>[_, +_]] {
  def deriveCovariant[A]: Covariant[({ type lambda[+B] = A :=> B })#lambda] =
    new Covariant[({ type lambda[+B] = A :=> B })#lambda] {
      def map[B, C](f: B => C): A :=> B => A :=> C = rightMap(f)
    }

  def rightMap[A, B, C](f: B => C): (A :=> B) => (A :=> C)
}

trait Bicovariant[:=>[+_, +_]] extends RightCovariant[:=>] {

  def bimap[A, B, AA, BB](f: A => AA, g: B => BB): (A :=> B) => (AA :=> BB)

  def leftMap[A, B, AA](f: A => AA): (A :=> B) => (AA :=> B)  =
    bimap(f, identity[B])
  def rightMap[A, B, BB](g: B => BB): (A :=> B) => (A :=> BB) =
    bimap(identity[A], g)
}

object Bicovariant {

  implicit val Tuple2Bicovariant: Bicovariant[Tuple2] =
    new Bicovariant[Tuple2] {
      override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): ((A, B)) => (AA, BB) = { case (a, b) => (f(a), g(b)) }
    }

  implicit val EitherBicovariant: Bicovariant[Either] =
    new Bicovariant[Either] {
      override def bimap[A, B, AA, BB](f: A => AA, g: B => BB): Either[A, B] => Either[AA, BB] = {
        case Right(a) => Right(g(a))
        case Left(b)  => Left(f(b))
      }
    }
}

trait BicovariantSyntax {

  implicit class BicovariantOps[:=>[+_, +_], A, B](f: => A :=> B) {

    def bimap[C, D](g: A => C, h: B => D)(implicit bicovariant: Bicovariant[:=>]): C :=> D =
      bicovariant.bimap(g, h)(f)

    def leftMap[C](ac: A => C)(implicit bicovariant: Bicovariant[:=>]): C :=> B =
      bicovariant.leftMap(ac)(f)

    def rightMap[C](bc: B => C)(implicit bicovariant: Bicovariant[:=>]): A :=> C =
      bicovariant.rightMap(bc)(f)
  }
}
