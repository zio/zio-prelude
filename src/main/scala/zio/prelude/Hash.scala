package zio.prelude

import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

trait HashLaws extends Lawful[Equal with Hash] {

  final val laws = consistencyLaw

  final val consistencyLaw = new Laws.Law2[Equal with Hash]("consistencyLaw") {
    def apply[A](a1: A, a2: A)(implicit caps: Equal[A] with Hash[A]): TestResult =
      (a1 === a2) <==> (Hash[A].hash(a1) === Hash[A].hash(a2))
  }
}

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

object Hash extends HashLaws {

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
