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
import zio.random.Random
import zio.test._

/**
 * Provides generators for data types from _ZIO Prelude_.
 */
object Gens {

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
   * A generator of state transition functions.
   */
  def state[R, S, A](s: Gen[R, S], a: Gen[R, A]): Gen[R, State[S, A]] =
    Gen.function[R, S, (S, A)](s <*> a).map(State.modify)

  /**
   * A generator of `Validation` values.
   */
  def validation[R <: Random with Sized, E, A](e: Gen[R, E], a: Gen[R, A]): Gen[R, Validation[E, A]] =
    Gen.either(Gens.parSeq(Gen.empty, e), a).map {
      case Left(es) => Validation.halt(es)
      case Right(a) => Validation.succeed(a)
    }
}
