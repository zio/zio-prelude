package zio.prelude

trait HashLaws[-A] {
  def hash(a: A): Int

  final def consistencyLaw[A1 <: A](a1: A1, a2: A1)(implicit equal: Equal[A1]): Boolean =
    (a1 === a2) <==> (hash(a1) === hash(a2))
}
sealed trait Hash[-A] extends HashLaws[A]
object Hash {
  def apply[A](implicit hash: Hash[A]): Hash[A] = hash

  def apply[A](f: A => Int): Hash[A] =
    new Hash[A] {
      def hash(a: A): Int = f(a)
    }

  def default[A]: Hash[A] = Hash[A]((a: A) => a.hashCode)

  implicit val stringHash: Hash[String] = Hash.default[String]
  implicit val intHash: Hash[Int]       = Hash.default[Int]
  implicit val longHash: Hash[Long]     = Hash.default[Long]
}
trait HashSyntax {
  implicit class HashSyntax[A](a: A) {
    def hash(implicit hash: Hash[A]): Int = hash.hash(a)
  }
}
