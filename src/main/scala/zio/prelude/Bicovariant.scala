package zio.prelude

trait RightMap[:=>[_,+_]] {
  def deriveCovariant[A]: Covariant[({ type lambda[+B] = A :=> B })#lambda] =
    new Covariant[({ type lambda[+B] = A :=> B })#lambda] {
      def map[B, C](f: B => C): A :=> B => A :=> C = rightMap(f)
    }

  def rightMap[A, B, C](f: B => C): (A :=> B) => (A :=> C)
}

trait Bicovariant[P[+_, +_]] extends RightMap[P] {

  def bimap[AA,A,B,BB](f: A => AA, g: B => BB): P[A,B] => P[AA,BB] =
    leftMap(f) andThen rightMap(g)

  def leftMap[A,B,C](f: A => C): P[A,B] => P[C,B]
}

object Bicovariant {

  implicit val Tuple2Bicovariant: Bicovariant[Tuple2] =
    new Bicovariant[Tuple2] {
      override def leftMap[A, B, C](f: A => C): ((A, B)) => (C, B) =
        { case(a,b) => (f(a),b) }
      override def rightMap[A, B, C](f: B => C): ((A, B)) => (A, C) =
        { case(a,b) => (a,f(b)) }
    }

  implicit val Either2Bicovariant: Bicovariant[Either] =
    new Bicovariant[Either] {
      override def leftMap[A, B, C](f: A => C): Either[A, B] => Either[C, B] = {
          case Left(e) => Left(f(e))
          case Right(v) => Right(v)
        }
      override def rightMap[A, B, C](f: B => C): Either[A, B] => Either[A, C] = {
        case Left(e) => Left(e)
        case Right(v) => Right(f(v))
      }
    }
}

trait BicovariantSyntax {

  implicit class BicovariantOps[:=>[+_, +_], A, B](f: => A :=> B) {

    def bimap[C, D](g: A => C, h: B => D)(implicit bicovariant: Bicovariant[:=>]): C :=> D =
      bicovariant.bimap(g, h)(f)

    def leftMap[C](ca: A => C)(implicit bicovariant: Bicovariant[:=>]): C :=> B =
      bicovariant.leftMap(ca)(f)

    def rightMap[C](bc: B => C)(implicit bicovariant: Bicovariant[:=>]): A :=> C =
      bicovariant.rightMap(bc)(f)
  }
}
