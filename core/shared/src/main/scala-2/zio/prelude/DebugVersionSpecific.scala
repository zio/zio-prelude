package zio.prelude

/**
 * Members of the `Debug` companion object that differ between Scala versions.
 * On Scala 3 it adds a `Debug` instance for `IArray`, rendered like an
 * `Array`: `IArray(1, 2)`. On Scala 2 it is empty.
 */
trait DebugVersionSpecific
