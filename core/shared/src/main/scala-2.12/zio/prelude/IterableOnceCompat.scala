package zio.prelude

/**
 * Needed because Scala 2.12 doesn't have `IterableOnce`
 */
private[prelude] trait IterableOnceCompat[+A] {

  /**
   * Copied from `IterableOnce#iterator`
   */
  def iterator: Iterator[A]

  /**
   * Adapted from `IterableOnce#knownSize`
   */
  def knownSize: Int

}
