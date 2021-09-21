package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object IdentityFlattenSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityFlattenSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(IdentityFlattenLaws)(GenF.chunk, Gen.chunkOf(Gen.anyInt))),
        test("either")(checkAllLaws(IdentityFlattenLaws)(GenFs.either(Gen.anyInt), Gen.anyInt)),
        test("list")(checkAllLaws(IdentityFlattenLaws)(GenF.list, Gen.anyInt)),
        test("option")(checkAllLaws(IdentityFlattenLaws)(GenF.option, Gen.anyInt)),
        test("vector")(checkAllLaws(IdentityFlattenLaws)(GenF.vector, Gen.anyInt))
      )
    )

}
