package zio.prelude

import zio.prelude.laws._
import zio.prelude.newtypes.{Prod, Sum}
import zio.test._
import zio.test.laws._

object NaturalSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("NaturalSpec")(
      suite("laws")(
        test("product commutative")(checkAllLaws(CommutativeLaws)(Gens.anyNatural.map(Prod(_)))),
        test("product identity")(checkAllLaws(IdentityLaws)(Gens.anyNatural.map(Prod(_)))),
        test("sum commutative")(checkAllLaws(CommutativeLaws)(Gens.anyNatural.map(Sum(_)))),
        test("sum inverse")(checkAllLaws(InverseLaws)(Gens.anyNatural.map(Sum(_))))
      )
    )

}
