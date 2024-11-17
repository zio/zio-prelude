package zio.prelude

/**
 * Needed because Scala 2.12 doesn't have `IterableOnce`
 */
private[prelude] trait IterableOnceCompat[+A] extends IterableOnce[A]
