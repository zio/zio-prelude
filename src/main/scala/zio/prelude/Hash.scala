package zio.prelude

//import java.util.{ Arrays => JArrays }
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * `Hash[A]` provides implicit evidence that a value of type `A` has a Hash(`A`)
 */
trait Hash[-A] extends Equal[A] { self =>
  def hash(a: A): Int

  //def both[B](that: Hash[B]): Hash[(A, B)] = (self bothWith that)(t => t._1 -> t._2)

//  def bothWith[B, C](that: Hash[B])(f: C => (A, B)): Hash[C] =
//    Hash { (c: C) =>
//      val (a, b) = f(c)
//
//      JArrays.hashCode(Array(self.hash(a), that.hash(b)))
//    }
//
//  def contramap[B](f: B => A): Hash[B] = Hash[B]((b: B) => self.hash(f(b)))
//
//  def either[B](that: Hash[B]): Hash[Either[A, B]] = (self eitherWith that)(identity)
//
//  def eitherWith[B, C](that: Hash[B])(f: C => Either[A, B]): Hash[C] =
//    Hash { (c: C) =>
//      // TODO: Use Either hash when it exists
//      f(c) match {
//        case Left(a)  => JArrays.hashCode(Array("Left".hashCode, self.hash(a)))
//        case Right(b) => JArrays.hashCode(Array("Right".hashCode, that.hash(b)))
//      }
//    }
//
//  def option: Hash[Option[A]] = self optionWith identity
//
//  def optionWith[B](f: B => Option[A]): Hash[B] =
//    Hash { b: B =>
//      f(b) match {
//        case Some(a) => JArrays.hashCode(Array("Some".hashCode, self.hash(a)))
//        case None    => None.hashCode
//      },
//      (b1: B, b2: B) =>
//    }
}

object Hash extends Lawful[Hash] {
  final val consistencyLaw = new Laws.Law2[Hash]("consistencyLaw") {
    def apply[A](a1: A, a2: A)(implicit caps: Hash[A]): TestResult =
      (a1 === a2) <==> (Hash[A].hash(a1) === Hash[A].hash(a2))
  }

  final val laws = consistencyLaw

  def apply[A](implicit hash: Hash[A]): Hash[A] = hash

  def apply[A](f: A => Int, eq0: (A, A) => Boolean): Hash[A] =
    new Hash[A] {
      def hash(a: A): Int = f(a)
      override def equal(l: A, r: A): Boolean = eq0(l, r)
    }

}

trait HashSyntax {

  implicit class HashSyntax[A](a: A) {
    def hash(implicit hash: Hash[A]): Int = hash.hash(a)

    def ##(implicit hash: Hash[A]): Int = hash.hash(a)
  }

}
