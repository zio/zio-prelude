package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.laws.GenF

/**
 * Provides higher kinded generators.
 */
object GenFs {

  def nonEmptyList: GenF[Random with Sized, NonEmptyList] =
    new GenF[Random with Sized, NonEmptyList] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, NonEmptyList[A]] =
        Gens.nonEmptyListOf(gen)
    }

  val option: GenF[Random, Option] =
    new GenF[Random, Option] {
      def apply[R1 <: Random, A](gen: Gen[R1, A]): Gen[R1, Option[A]] =
        Gen.option(gen)
    }

  val list: GenF[Random with Sized, List] =
    new GenF[Random with Sized, List] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, List[A]] =
        Gen.listOf(gen)
    }

  val vector: GenF[Random with Sized, Vector] =
    new GenF[Random with Sized, Vector] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, Vector[A]] =
        Gen.vectorOf(gen)
    }

  val set: GenF[Random with Sized, Set] =
    new GenF[Random with Sized, Set] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, Set[A]] =
        Gen.setOf(gen)
    }

  def tuple2[R <: Random with Sized, A](a: Gen[R, A]): GenF[R, ({ type lambda[+x] = (A, x) })#lambda] =
    new GenF[R, ({ type lambda[+x] = (A, x) })#lambda] {
      def apply[R1 <: R, B](b: Gen[R1, B]): Gen[R1, (A, B)] =
        a.zip(b)
    }

  def tuple3[R <: Random with Sized, A, B](a: Gen[R, (A, B)]): GenF[R, ({ type lambda[+x] = ((A, B), x) })#lambda] =
    new GenF[R, ({ type lambda[+x] = ((A, B), x) })#lambda] {
      def apply[R1 <: R, C](b: Gen[R1, C]): Gen[R1, ((A, B), C)] =
        a.zip(b)
    }

  def map[R <: Random with Sized, K](k: Gen[R, K]): GenF[R, ({ type lambda[+x] = Map[K, x] })#lambda] =
    new GenF[R, ({ type lambda[+x] = Map[K, x] })#lambda] {
      def apply[R1 <: R, V](v: Gen[R1, V]): Gen[R1, Map[K, V]] =
        Gen.mapOf(k, v)
    }

  def either[R <: Random with Sized, A](e: Gen[R, A]): GenF[R, ({ type lambda[+x] = Either[A, x] })#lambda] =
    new GenF[R, ({ type lambda[+x] = Either[A, x] })#lambda] {
      def apply[R1 <: R, B](a: Gen[R1, B]): Gen[R1, Either[A, B]] =
        Gen.either(e, a)
    }

  /**
   * A generator of validation values.
   */
  def validation[R <: Random with Sized, E](e: Gen[R, E]): GenF[R, ({ type lambda[+x] = Validation[E, x] })#lambda] =
    new GenF[R, ({ type lambda[+x] = Validation[E, x] })#lambda] {
      def apply[R1 <: R, A](a: Gen[R1, A]): Gen[R1, Validation[E, A]] =
        Gens.validation(e, a)
    }
}
