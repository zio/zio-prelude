package zio.prelude

/**
 * `Hash[A]` provides implicit evidence that a value of type `A` has a Hash(`A`)
 */
sealed trait Hash[-A] { self =>
  def hash(a: A): Int

  final def concat[B: Hash](a: A, b: B): Int = self.hash(a) ^ b.hash
}

object Hash {
  def apply[A](implicit hash: Hash[A]): Hash[A] = hash

  def apply[A](f: A => Int): Hash[A] =
    new Hash[A] {
      def hash(a: A): Int = f(a)
    }

  def default[A]: Hash[A] = Hash[A]((a: A) => a.hashCode)

  def ordered[A: Hash]: Hash[Iterable[A]] =
    Hash(_.foldLeft((Iterable.hashCode, 0)) {
      case ((h, i), a) => (h concat a concat i , i + 1)
    }._2)

  def unordered[A: Hash]: Hash[Iterable[A]] =
    Hash(_.foldLeft(Iterable.hashCode)(_ concat _))

  implicit val booleanHash: Hash[Boolean] = Hash.default[Boolean]

  implicit val byteHash: Hash[Byte] = Hash.default[Byte]

  implicit val charHash: Hash[Char] = Hash.default[Char]

  implicit val doubleHash: Hash[Double] = Hash.default[Double]

  implicit def eitherHash[L: Hash, R: Hash]: Hash[Either[L, R]] =
    Hash(_ match {
      case Left(l)  => l.concat(Left(()).hashCode)
      case Right(r) => r.concat(Right(()).hashCode)
    })

  implicit val floatHash: Hash[Float] = Hash.default[Float]

  implicit val intHash: Hash[Int] = Hash.default[Int]

  implicit def listHash[A: Hash]: Hash[List[A]] = Hash.ordered[A]

  implicit val longHash: Hash[Long] = Hash.default[Long]

  implicit def mapHash[K: Hash, V: Hash]: Hash[Map[K, V]] = Hash.unOrdered[(K, V)]

  implicit def optionHash[A: Hash]: Hash[Option[A]] =
    Hash(_ match {
      case Some(a) => Some(()).hashCode concat a
      case None    => None.hashCode
    })

  implicit def setHash[A: Hash]: Hash[Set[A]] = Hash.unOrdered[A]

  implicit val stringHash: Hash[String] = Hash.default[String]

  implicit def tuple2[A: Hash, B: Hash]: Hash[(A, B)] = Hash((ab: (A, B)) => ab._1.hash ^ ab._2.hash)

  implicit val unitHash: Hash[Unit] = Hash.default[Unit]

  implicit def vectorHash[A: Hash]: Hash[Vector[A]] = Hash.ordered[A]
}

trait HashSyntax {

  implicit class HashSyntax[A](a: A) {
    def hash(implicit hash: Hash[A]): Int = hash.hash(a)

    def ##(implicit hash: Hash[A]): Int = hash.hash(a)

    def concat[B: Hash](b: B)(implicit hash: Hash[A]): Int = hash.concat(a, b)

  }

}
