package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object AssociativeFlattenSpec extends ZIOSpecDefault {

  def spec: ZSpec[Environment, Nothing] =
    suite("AssociativeFlattenSpec")(
      suite("laws")(
        test("map")(checkAllLaws(AssociativeFlattenLaws)(GenFs.map(Gen.int), Gen.int))
      )
    )
}
