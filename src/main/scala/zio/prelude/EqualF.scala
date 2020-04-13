package zio.prelude

/**
 * `EqualF[F]` represents a universally quantified function from `Equal[A]` to
 * `Equal[F[A]]` for some `F[_]`. You can think of `EqualF` as a "recipe" for
 * building an `EqualF[A]]` instance given an `Equal[A]`.
 *
 * For example, if we know how to compare values of type `A` for equality then
 * we can compare lists with elements of type `A` for equality by checking
 * that the length of the lists is the same and each pair of corresponding
 * elements are equal. And we can do this for any type `A` as long as it has
 * an `Equal` instance.
 *
 * This is used by the library to derive `Equal` instances for higher kinded
 * types given `Equal` instances for the type they are parameterized on.
 */
trait EqualF[F[_]] {

  /**
   * Derive an `Equal[F[A]]` given an `Equal[A]`.
   */
  def deriveEqual[A](equal: Equal[A]): Equal[F[A]]
}

object EqualF {

  /**
   * Summon an implicit `EqualF[F]`
   */
  def apply[F[_]](implicit equalF: EqualF[F]): EqualF[F] =
    equalF

  /**
   * The `EqualF` instance for `Option`.
   */
  implicit val OptionEqualF: EqualF[Option] =
    new EqualF[Option] {
      def deriveEqual[A](A: Equal[A]): Equal[Option[A]] =
        Equal.OptionEqual(A)
    }
}
