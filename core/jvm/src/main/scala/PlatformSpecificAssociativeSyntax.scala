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
