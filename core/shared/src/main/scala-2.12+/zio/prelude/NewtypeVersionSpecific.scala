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
 * Declares the abstract `Type` of a Scala 2 [[Newtype]]. The Scala 3 `Newtype`
 * declares `Type` itself and does not extend this trait.
 */
trait NewtypeVersionSpecific {

  /**
   * The newtype. Declare a type alias for it next to the newtype object, for
   * example `type Meter = Meter.Type`.
   */
  type Type
}
