package zio.prelude

import Debug.*

/**
 * Members of the `Debug` companion object that differ between Scala versions.
 * On Scala 3 it adds a `Debug` instance for `IArray`, rendered like an
 * `Array`: `IArray(1, 2)`. On Scala 2 it is empty.
 */
trait DebugVersionSpecific:

  /**
   * Derives a `Debug[IArray[A]]` given a `Debug[A]`.
   */
  given[A](using Debug[A]): Debug[IArray[A]] with
    def debug(array: IArray[A]): Repr =
      Repr.VConstructor(List("scala"), "IArray", array.toList.map(_.debug))
