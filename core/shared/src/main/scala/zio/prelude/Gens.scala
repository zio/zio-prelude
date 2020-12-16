package zio.prelude

import zio.prelude.fx.Cause
import zio.random.Random
import zio.test._

/**
 * Provides generators for data types from _ZIO Prelude_.
 */
object Gens {

  def semiring[R <: Random with Sized, E](e: Gen[R, E]): Gen[R, Cause[E]] = {
    val failure = e.map(Cause.single)

    def sequential(n: Int) = Gen.suspend {
      for {
        i <- Gen.int(1, n - 1)
        l <- causesN(i)
        r <- causesN(n - i)
      } yield Cause.Then(l, r)
    }

    def parallel(n: Int) = Gen.suspend {
      for {
        i <- Gen.int(1, n - 1)
        l <- causesN(i)
        r <- causesN(n - i)
      } yield Cause.Both(l, r)
    }

    def causesN(n: Int): Gen[R, Cause[E]] = Gen.suspend {
      if (n == 1) failure
      else Gen.oneOf(sequential(n), parallel(n))
    }

    Gen.small(causesN, 1)
  }

  /**
   * A generator of `NonEmptyList` values.
   */
  def nonEmptyListOf[R <: Random with Sized, A](a: Gen[R, A]): Gen[R, NonEmptyList[A]] =
    Gen.listOf1(a).map(NonEmptyList.fromCons)

  /**
   * A generator of state transition functions.
   */
  def state[R, S, A](s: Gen[R, S], a: Gen[R, A]): Gen[R, State[S, A]] =
    Gen.function[R, S, (S, A)](s <*> a).map(State.modify)

  /**
   * A generator of `Validation` values.
   */
  def validation[R <: Random with Sized, E, A](e: Gen[R, E], a: Gen[R, A]): Gen[R, Validation[E, A]] =
    Gen.either(Gens.semiring(e), a).map {
      case Left(es) => Validation.halt(es)
      case Right(a) => Validation.succeed(a)
    }
}
