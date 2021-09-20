package zio.prelude

import zio.prelude.laws._
import zio.prelude.newtypes.{Prod, Sum}
import zio.test._
import zio.test.laws._

object NaturalSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("NaturalSpec")(
      suite("laws")(
        testM("product commutative")(checkAllLaws(CommutativeLaws)(Gens.anyNatural.map(Prod(_)))),
        testM("product identity")(checkAllLaws(IdentityLaws)(Gens.anyNatural.map(Prod(_)))),
        testM("sum commutative")(checkAllLaws(CommutativeLaws)(Gens.anyNatural.map(Sum(_)))),
        testM("sum inverse")(checkAllLaws(InverseLaws)(Gens.anyNatural.map(Sum(_))))
      )
    )

}
