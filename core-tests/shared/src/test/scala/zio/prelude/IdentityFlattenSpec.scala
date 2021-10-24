package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object IdentityFlattenSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityFlattenSpec")(
      suite("laws")(
        testM("chunk")(checkAllLaws(IdentityFlattenLaws)(GenF.chunk, Gen.chunkOf(Gen.anyInt))),
        testM("either")(checkAllLaws(IdentityFlattenLaws)(GenFs.either(Gen.anyInt), Gen.anyInt)),
        testM("list")(checkAllLaws(IdentityFlattenLaws)(GenF.list, Gen.anyInt)),
        testM("option")(checkAllLaws(IdentityFlattenLaws)(GenF.option, Gen.anyInt)),
        testM("vector")(checkAllLaws(IdentityFlattenLaws)(GenF.vector, Gen.anyInt))
      )
    )

}
