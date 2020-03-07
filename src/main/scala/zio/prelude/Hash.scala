package zio.prelude

/**
 * `Hash[A]` provides implicit evidence that a value of type `A` has a Hash(`A`)
 */
sealed trait Hash[-A] { self =>
  def hash(a: A): Int

  def both[B](that: Hash[B]): Hash[(A, B)] = (self bothWith that)(t => t._1 -> t._2)

  def bothWith[B, C](that: Hash[B])(f: C => (A, B)): Hash[C] =
    Hash { (c: C) =>
      // TODO: Use Tuple hash when it exists
      val (a, b) = f(c)

      java.util.Arrays.hashCode(Array(self.hash(a), that.hash(b)))
    }

  def contramap[B](f: B => A): Hash[B] = Hash[B]((b: B) => self.hash(f(b)))

  def either[B](that: Hash[B]): Hash[Either[A, B]] = (self eitherWith that)(identity)

  def eitherWith[B, C](that: Hash[B])(f: C => Either[A, B]): Hash[C] =
    Hash { (c: C) =>
      // TODO: Use Either hash when it exists
      f(c) match {
        case Left(a)  => java.util.Arrays.hashCode(Array("Left".hashCode, self.hash(a)))
        case Right(b) => java.util.Arrays.hashCode(Array("Right".hashCode, that.hash(b)))
      }
    }
}

object Hash {
  def apply[A](implicit hash: Hash[A]): Hash[A] = hash

  def apply[A](f: A => Int): Hash[A] =
    new Hash[A] {
      def hash(a: A): Int = f(a)
    }

  def default[A]: Hash[A] = Hash[A]((a: A) => a.hashCode)

  implicit def arrayHash[A: Hash]: Hash[Array[A]] = Hash(_.foldLeft(Array.empty.hashCode)(_ combine _))

  implicit val booleanHash: Hash[Boolean] = Hash.default[Boolean]

  implicit val byteHash: Hash[Byte] = Hash.default[Byte]

  implicit val charHash: Hash[Char] = Hash.default[Char]

  implicit val doubleHash: Hash[Double] = Hash.default[Double]

  implicit def eitherHash[L: Hash, R: Hash]: Hash[Either[L, R]] =
    Hash(_ match {
      case Left(l)  => l.combine(Left(()).hashCode)
      case Right(r) => r.combine(Right(()).hashCode)
    })

  implicit val floatHash: Hash[Float] = Hash.default[Float]

  implicit val intHash: Hash[Int] = Hash.default[Int]

  implicit def listHash[A: Hash]: Hash[List[A]] = Hash.default[List[A]]

  implicit val longHash: Hash[Long] = Hash.default[Long]

  implicit def mapHash[K: Hash, V: Hash]: Hash[Map[K, V]] = Hash.default[Map[K, V]]

  implicit def optionHash[A: Hash]: Hash[Option[A]] =
    Hash(_ match {
      case Some(a) => a combine Some(()).hashCode
      case None    => None.hashCode
    })

  implicit def setHash[A: Hash]: Hash[Set[A]] = Hash.default[Set[A]]

  implicit val stringHash: Hash[String] = Hash.default[String]

  implicit def tuple2[A: Hash, B: Hash]: Hash[(A, B)] = Hash((ab: (A, B)) => ab._1 combine ab._2)

  implicit val unitHash: Hash[Unit] = Hash.default[Unit]

  implicit def vectorHash[A: Hash]: Hash[Vector[A]] = Hash.default[Vector[A]]
}

trait HashSyntax {

  implicit class HashSyntax[A](a: A) {
    def hash(implicit hash: Hash[A]): Int = hash.hash(a)

    def ##(implicit hash: Hash[A]): Int = hash.hash(a)

  }

}
