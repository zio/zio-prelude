package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object IdentityFlattenSpec extends ZIOSpecDefault {

  def spec: ZSpec[Environment, Nothing] =
    suite("IdentityFlattenSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(IdentityFlattenLaws)(GenF.chunk, Gen.chunkOf(Gen.int))),
        test("either")(checkAllLaws(IdentityFlattenLaws)(GenFs.either(Gen.int), Gen.int)),
        test("list")(checkAllLaws(IdentityFlattenLaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(IdentityFlattenLaws)(GenF.option, Gen.int)),
        test("vector")(checkAllLaws(IdentityFlattenLaws)(GenF.vector, Gen.int))
      )
    )

}
