package zio.prelude

import zio.prelude.newtypes.{Prod, Sum}
import zio.test._
import zio.test.laws._

object NaturalSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("NaturalSpec")(
      suite("laws")(
        testM("product commutative")(checkAllLaws(Commutative)(Gens.anyNatural.map(Prod(_)))),
        testM("product identity")(checkAllLaws(Identity)(Gens.anyNatural.map(Prod(_)))),
        testM("sum commutative")(checkAllLaws(Commutative)(Gens.anyNatural.map(Sum(_)))),
        testM("sum inverse")(checkAllLaws(Inverse)(Gens.anyNatural.map(Sum(_))))
      )
    )

}
