package zio.prelude
package experimental

import zio.prelude.experimental.laws._
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

import scala.util.control.NoStackTrace

object DistributiveProdSpec extends ZIOBaseSpec {

  def spec: Spec[Environment, Any] =
    suite("DistributiveProdSpec")(
      suite("laws")(
        test("Cause distributive multiply")(
          checkAllLaws(DistributiveProdLaws)(Gen.causes(Gen.int, Gen.const(new Throwable with NoStackTrace {})))
        ),
        test("ParSeq distributive multiply")(checkAllLaws(DistributiveProdLaws)(Gens.parSeq(Gen.unit, Gen.int)))
      )
    )
}
