package zio.prelude

/**
 * Needed because Scala 2.12 doesn't have `IterableOnce`
 *
 * TODO: Remove when support for Scala 2.12 is dropped
 */
private[prelude] trait IterableOnceCompat[+A] extends IterableOnce[A]
