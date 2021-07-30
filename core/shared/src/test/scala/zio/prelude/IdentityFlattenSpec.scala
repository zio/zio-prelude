package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityFlattenSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityFlattenSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(IdentityFlatten)(GenF.chunk, Gen.chunkOf(Gen.anyInt))),
        test("either")(checkAllLaws(IdentityFlatten)(GenFs.either(Gen.anyInt), Gen.anyInt)),
        test("list")(checkAllLaws(IdentityFlatten)(GenF.list, Gen.anyInt)),
        test("option")(checkAllLaws(IdentityFlatten)(GenF.option, Gen.anyInt)),
        test("vector")(checkAllLaws(IdentityFlatten)(GenF.vector, Gen.anyInt))
      )
    )

}
