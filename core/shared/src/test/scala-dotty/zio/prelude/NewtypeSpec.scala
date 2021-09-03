package zio.prelude

import zio.NonEmptyChunk
import zio.prelude.Refinement.{And => _, Or => _}
import zio.prelude.newtypes._
import zio.test.Assertion._
import zio.test.AssertionM.Render.param
import zio.test._

object NewtypeSpecTypes {
  type Natural = Natural.Type
  object Natural extends Subtype[Int] {
    override inline def refinement = Refinement.greaterThanOrEqualTo(0)

    val two: Natural = Natural(2)

    def unsafeWrap(int: Int): Natural = wrap(int)
  }
}
