package zio.prelude
package experimental

/**
 * An experimental marker type class for an [[Absorption]] lattice whose join
 * and meet distribute over each other: `a1 vvv (a2 ^^^ a3)` equals
 * `(a1 vvv a2) ^^^ (a1 vvv a3)`, and the same with `vvv` and `^^^` exchanged.
 * It adds no members. It lives in the experimental module, so its name and
 * shape can change between releases.
 *
 * The built-in instances are for `Boolean` and `Set[A]`, in the `Absorption`
 * companion. `DistributiveAbsorptionLaws` in zio-prelude-experimental-laws
 * checks the laws.
 */
trait DistributiveAbsorption[A] extends Absorption[A]

/**
 * Companion of [[DistributiveAbsorption]], with the summoner.
 */
object DistributiveAbsorption {

  /**
   * Summons an implicit `DistributiveAbsorption[A]`.
   */
  def apply[A](implicit distributiveAbsorption: DistributiveAbsorption[A]): DistributiveAbsorption[A] =
    distributiveAbsorption
}
