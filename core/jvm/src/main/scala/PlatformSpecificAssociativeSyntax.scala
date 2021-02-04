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

trait PlatformSpecificAssociativeSyntax {

  /**
   * Provides platform specific syntax for combining values
   * in a `ParIterable` which is thus done in a parallel manner.
   */
  implicit class PlatformSpecificAssociativeParIterableOps[A](
    private val p: collection.parallel.immutable.ParIterable[A]
  ) {

    /**
     * Associatively combines the values in a parallel manner,
     * while blocking the thread.
     */
    def reduceAssociative(implicit associative: Associative[A]): Option[A] =
      p.reduceOption(associative.combine(_, _))

    /**
     * Returns an effect, that associatively combines the values in a parallel manner,
     * while ensuring the current thread isn't blocked.
     */
    def reduceAssociativeM(implicit associative: Associative[A]): zio.RIO[zio.blocking.Blocking, Option[A]] =
      zio.blocking.effectBlocking(reduceAssociative)
  }

}
