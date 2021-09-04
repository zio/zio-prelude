package zio.prelude.newtypes

import zio.prelude._

object Natural extends Subtype[Int] {

  // scalafix:off
  def refinement = refine(Refinement.greaterThanOrEqualTo(0))
  // scalafix:on

  val one: Natural =
    Natural(1)

  val zero: Natural =
    Natural(0)

  def successor(n: Natural): Natural =
    wrap(n + 1)

  def times(x: Natural, y: Natural): Natural = {
    val product = x * y
    if (x == 0 || product / x != y) wrap(Int.MaxValue) else wrap(product)
  }

  def plus(x: Natural, y: Natural): Natural = {
    val sum = x + y
    if (sum < 0) wrap(Int.MaxValue) else wrap(sum)
  }

  def minus(x: Natural, y: Natural): Natural = {
    val difference = x - y
    if (difference < 0) zero else wrap(difference)
  }

  private[prelude] def unsafeMake(n: Int): Natural =
    wrap(n)
}
