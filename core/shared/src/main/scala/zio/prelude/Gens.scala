/*
 * Copyright 2020-2021 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.prelude

import zio.prelude.fx.Cause
import zio.prelude.newtypes._
import zio.random.Random
import zio.test._

/**
 * Provides generators for data types from _ZIO Prelude_.
 */
object Gens {

  /**
   * A generator of natural numbers. Shrinks toward '0'.
   */
  val anyNatural: Gen[Random, Natural] =
    natural(Natural.zero, Natural.unsafeApply(Int.MaxValue))

  /**
   * A generator of natural numbers inside the specified range: [start, end].
   * The shrinker will shrink toward the lower end of the range ("smallest").
   */
  def natural(min: Natural, max: Natural): Gen[Random, Natural] =
    Gen.int(min, max).map(Natural.unsafeApply)

  def parSeq[R <: Random with Sized, Z <: Unit, A](z: Gen[R, Z], a: Gen[R, A]): Gen[R, ParSeq[Z, A]] = {
    val failure = a.map(Cause.single)
    val empty   = z.map(_ => Cause.empty.asInstanceOf[ParSeq[Nothing, Nothing]])

    def sequential(n: Int) = Gen.suspend {
      for {
        i <- Gen.int(1, n - 1)
        l <- parSeqN(i)
        r <- parSeqN(n - i)
      } yield Cause.Then(l, r)
    }

    def parallel(n: Int) = Gen.suspend {
      for {
        i <- Gen.int(1, n - 1)
        l <- parSeqN(i)
        r <- parSeqN(n - i)
      } yield Cause.Both(l, r)
    }

    def parSeqN(n: Int): Gen[R, ParSeq[Z, A]] = Gen.suspend {
      if (n == 1) Gen.oneOf(empty, failure)
      else Gen.oneOf(sequential(n), parallel(n))
    }

    Gen.small(parSeqN, 1)
  }

  /**
   * A generator of `NonEmptyList` values.
   */
  def nonEmptyListOf[R <: Random with Sized, A](a: Gen[R, A]): Gen[R, NonEmptyList[A]] =
    Gen.listOf1(a).map(NonEmptyList.fromCons)

  /**
   * A generator of `NonEmptyMultiSet` values.
   */
  def nonEmptyMultiSetOf[R <: Random with Sized, A](a: Gen[R, A]): Gen[R, NonEmptyMultiSet[A]] =
    Gen.mapOf1(a, Gen.size.map(Natural.unsafeApply)).map(NonEmptyMultiSet.fromMapOption(_).get)

  /**
   * A generator of state transition functions.
   */
  def state[R, S, A](s: Gen[R, S], a: Gen[R, A]): Gen[R, State[S, A]] =
    Gen.function[R, S, (S, A)](s <*> a).map(State.modify)

  /**
   * A generator of `Validation` values.
   */
  def validation[R <: Random with Sized, W, E, A](
    w: Gen[R, W],
    e: Gen[R, E],
    a: Gen[R, A]
  ): Gen[R, ZValidation[W, E, A]] =
    Gen.chunkOf(w).flatMap { w =>
      Gen.either(Gen.chunkOf1(e), a).map {
        case Left(e)  => Validation.Failure(w, e)
        case Right(a) => Validation.Success(w, a)
      }
    }
}
