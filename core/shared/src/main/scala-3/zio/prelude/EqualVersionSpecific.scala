/*
 * Copyright 2023 John A. De Goes and the ZIO Contributors
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

import scala.deriving.*
import scala.compiletime.{erasedValue, summonFrom, summonInline}

/**
 * Members of the `Equal` companion object that differ between Scala versions.
 * On Scala 3 it adds `derived`, so a case class, sealed trait or enum can
 * derive an instance with `derives Equal`. On Scala 2 it is empty.
 */
trait EqualVersionSpecific {

  /**
   * Derives an `Equal[A]` for a case class, sealed trait or enum, so it can be
   * used with `derives Equal`. Two case class values are equal when each pair
   * of fields is equal with the field type's `Equal` instance. Two values of a
   * sealed trait or enum are equal when they are the same case and that case's
   * values are equal. A case whose type is a subtype of `A` gets a derived
   * instance; every other field type needs an `Equal` instance in scope, or the
   * derivation does not compile.
   *
   * {{{
   * // scala-3-only
   * final case class Point(x: Int, y: Int) derives Equal
   *
   * val same: Boolean = Point(1, 2) === Point(1, 2)
   * // true
   * }}}
   */
  inline def derived[A](using mirror: Mirror.Of[A]): Equal[A] =
    lazy val instances = summonAll[A, mirror.MirroredElemTypes]
    inline mirror match
      case sum: Mirror.SumOf[A] => equalSum(sum, instances)
      case product: Mirror.ProductOf[A] => equalProduct(product, instances)

  private inline def summonAll[A0, A <: Tuple]: List[Equal[_]] =
    inline erasedValue[A] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summon[A0, t] :: summonAll[A0, ts]
    }

  private inline def summon[A0, A]: Equal[A] =
    compiletime.summonFrom {
      case _: (A =:= A0) => compiletime.error("Cannot derive Equal.")
      case _: (A <:< A0) => Equal.derived(using summonInline[Mirror.Of[A]])
      case equal: Equal[A] => equal
    }

  private def equalSum[A](sum: Mirror.SumOf[A], elems: => List[Equal[_]]): Equal[A] =
    Equal.make { (left, right) =>
      val leftOrdinal = sum.ordinal(left)
      val rightOrdinal = sum.ordinal(right)
      (leftOrdinal == rightOrdinal) && check(elems(leftOrdinal))(left, right)
    }

  private def equalProduct[A](product: Mirror.ProductOf[A], elems: => List[Equal[_]]): Equal[A] =
    Equal.make { (left, right) =>
      iterator(left).zip(iterator(right)).zip(elems.iterator).forall {
        case ((l, r), elem) => check(elem)(l, r)
      }
    }

  private def check(equal: Equal[_])(left: Any, right: Any): Boolean =
    equal.asInstanceOf[Equal[Any]].equal(left, right)

  private def iterator[A](a: A) =
    a.asInstanceOf[Product].productIterator
}
