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

trait EqualVersionSpecific {

  inline def derived[A](using mirror: Mirror.Of[A]): Equal[A] =
    val instances = summonAll[A, mirror.MirroredElemTypes]
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

  private def equalSum[A](sum: Mirror.SumOf[A], elems: List[Equal[_]]): Equal[A] =
    Equal.make { (left, right) =>
      val leftOrdinal = sum.ordinal(left)
      val rightOrdinal = sum.ordinal(right)
      (leftOrdinal == rightOrdinal) && check(elems(leftOrdinal))(left, right)
    }

  private def equalProduct[A](product: Mirror.ProductOf[A], elems: List[Equal[_]]): Equal[A] =
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
