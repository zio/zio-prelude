package zio.prelude

/**
 * `Hash[A]` provides implicit evidence that a value of type `A` hash a Hash(`A`)
 */
sealed trait Hash[-A] { self =>
  def hash (a: A): Int
}

object Hash {
  def apply[A] (implicit hash: Hash[A]): Hash[A] = hash

  def apply[A] (f: A => Int): Hash[A] =
    new Hash[A] {
      def hash (a: A): Int = f(a)
    }

  def default[A]: Hash[A] = Hash[A]((a: A) => a.hashCode)

  implicit val booleanHash: Hash[Boolean] = Hash.default[Boolean]

  implicit val byteHash: Hash[Byte] = Hash.default[Byte]

  implicit val charHash: Hash[Char] = Hash.default[Char]

  implicit val doubleHash: Hash[Double] = Hash.default[Double]

  implicit def eitherHash[L: Hash, R: Hash]: Hash[Either[L, R]] = Hash.default[Either[L, R]]

  implicit val floatHash: Hash[Float] = Hash.default[Float]

  implicit val intHash: Hash[Int] = Hash.default[Int]

  implicit def listHash[A: Hash]: Hash[List[A]] = Hash.default[List[A]]

  implicit val longHash: Hash[Long] = Hash.default[Long]

  implicit def mapHash[K: Hash, V: Hash]: Hash[Map[K, V]] = Hash.default[Map[K, V]]

  implicit def optionHash[A: Hash]: Hash[Option[A]] = Hash.default[Option[A]]

  implicit def setHash[A: Hash]: Hash[Set[A]] = Hash.default[Set[A]]

  implicit val stringHash: Hash[String] = Hash.default[String]

  implicit val unitHash: Hash[Unit] = Hash.default[Unit]

  implicit def vectorHash[A: Hash]: Hash[Vector[A]] = Hash.default[Vector[A]]
}

trait HashSyntax {

  implicit class HashSyntax[A] (a: A) {
    def hash (implicit hash: Hash[A]): Int = hash.hash(a)

    def ## (implicit hash: Hash[A]): Int = hash.hash(a)
  }
}
