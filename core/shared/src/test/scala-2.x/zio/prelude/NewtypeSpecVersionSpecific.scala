package zio.prelude

object NewtypeSpecTypes {
  type Natural = Natural.Type
  object Natural extends Subtype[Int] {
    def refinement =
      refine(Refinement.greaterThanOrEqualTo(0))

    val two: Natural = Natural(2)

    def unsafeWrap(int: Int): Natural = wrap(int)
  }
}
