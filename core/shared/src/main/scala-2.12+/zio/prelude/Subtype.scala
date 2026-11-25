/*
 * Copyright 2020-2023 John A. De Goes and the ZIO Contributors
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

/**
 * The base class for newtypes that are also subtypes of their underlying type
 * `A`. A value of the subtype can be passed where an `A` is expected, and it
 * keeps the methods of `A` and the instances of contravariant type classes
 * such as `Equal` and `Ord`. A plain `A` still can't be passed where the
 * subtype is expected. Operations inherited from `A` return an `A`, so wrap
 * their result again to get the subtype back.
 *
 * Everything else works as for [[Newtype]], including smart subtypes that
 * define an `assertion`.
 *
 * {{{
 * object Count extends Subtype[Int]
 * type Count = Count.Type
 *
 * val three: Count = Count(3)
 * val next: Int    = three + 1 // 4
 * val isEqual      = three === Count(3) // true
 * }}}
 *
 * @see [[https://zio.dev/zio-prelude/newtypes/ New Types]]
 */
abstract class Subtype[A] extends Newtype[A] {

  /**
   * The subtype, which is a subtype of the underlying type `A`.
   */
  type Type <: A
}
