package zio.prelude

trait PlatformSpecificIdentitySyntax {

  /**
   * Provides platform specific syntax for combining values
   * in a `ParIterable` which is thus done in a parallel manner.
   */
  implicit class PlatformSpecificIdentityParIterableOps[A](
    private val p: collection.parallel.immutable.ParIterable[A]
  ) {

    /**
     * Associatively combines the values in a parallel manner, returning the `identity` element if empty,
     * while blocking the thread.
     */
    def reduceIdentity(implicit identity: Identity[A]): A =
      p.fold(identity.identity)(identity.combine(_, _))

    /**
     * Returns an effect, that associatively combines the values in a parallel manner, returning the `identity` element if empty,
     * while ensuring the current thread isn't blocked.
     */
    def reduceIdentityM(implicit identity: Identity[A]): zio.RIO[zio.blocking.Blocking, A] =
      zio.blocking.effectBlocking(reduceIdentity)
  }

}
