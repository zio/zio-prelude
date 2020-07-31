package zio.prelude

import zio.test._
import zio.test.laws._

object IdentityFlattenSpec extends DefaultRunnableSpec {

  def spec = suite("IdentityFlattenSpec")(
    suite("laws")(
      testM("chunk")(checkAllLaws(IdentityFlatten)(GenF.chunk, Gen.chunkOf(Gen.anyString))),
      testM("either")(checkAllLaws(IdentityFlatten)(GenFs.either(Gen.anyInt), Gen.anyInt)),
      testM("option")(checkAllLaws(IdentityFlatten)(GenF.option, Gen.anyInt)),
      testM("list")(checkAllLaws(IdentityFlatten)(GenF.list, Gen.anyString)),
      testM("vector")(checkAllLaws(IdentityFlatten)(GenF.vector, Gen.anyString))
    )
  )

}
