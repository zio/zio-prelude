package zio.prelude

/**
 * `Hash[A]` provides implicit evidence that a value of type `A` has a Hash(`A`)
 */
sealed trait Hash[-A] { self =>
  def hash(a: A): Int

  final def combine[B: Hash](a: A, b: B): Int = {
    val lhs = self.hash(a)
    lhs ^ (b.hash + 0x9e3779b9 + (lhs << 6) + (lhs >> 2))
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

    def combine[B: Hash](b: B)(implicit hash: Hash[A]): Int = hash.combine(a, b)
  }

}
