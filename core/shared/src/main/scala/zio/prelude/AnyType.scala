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
 * A type class with an instance for every type `A`. It stands for "no
 * constraint" where an abstraction takes a constraint as a type parameter:
 * `Covariant[F]` is a `CovariantSubset[F, AnyType]`, so its `mapSubset`
 * accepts any result type.
 */
sealed trait AnyType[A]

/** Provides the `AnyType` instance for every type. */
object AnyType {

  /** Returns an `AnyType[A]` for the type `A`; it is found implicitly for every `A`. */
  implicit def apply[A]: AnyType[A] = new AnyType[A] {}
}
