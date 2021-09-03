package zio.prelude

object NewtypeSpecTypes {

  type Natural = Natural.Type
  object Natural extends Subtype[Int] {
    // scalafix:off
    def refinement =
      refine(Refinement.greaterThanOrEqualTo(0))
    // scalafix:on

    val two: Natural = Natural(2)

    def unsafeWrap(int: Int): Natural = wrap(int)
  }

}
