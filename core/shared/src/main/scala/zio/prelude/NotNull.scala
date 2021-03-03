package zio.prelude

/**
 * Proof that `A` does not have type `Null`.
 *
 * Ambiguous implicits trick ensures that there is not an instance for `Null`.
 * [[https://gist.github.com/milessabin/de58f3ba7024d51dcc1a More info]]
 */
sealed trait NotNull[A]

object NotNull {

  /**
   * Since NotNull is just a marker trait with no functionality, it's safe to
   * reuse a single instance of it. This helps prevent unnecessary allocations.
   */
  private[this] val AnyNotNull: NotNull[Any] = new NotNull[Any] {}

  private[this] def exception: Exception =
    new Exception(
      "An instance of NotNull[Null] was used. This should never happen. Both ambiguous NotNull[Null] instances should always be in scope if one of them is."
    )

  implicit def `If you are seeing this, you probably need to add an explicit type parameter somewhere, because Null is being inferred.`
    : NotNull[Null] = throw exception

  implicit def NotNullNullAmbiguous: NotNull[Null] = throw exception

  implicit def ANotNull[A]: NotNull[A] = AnyNotNull.asInstanceOf[NotNull[A]]
}
