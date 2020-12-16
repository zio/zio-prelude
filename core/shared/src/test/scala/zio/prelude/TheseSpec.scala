package zio.prelude

import zio.test._
import zio.test.laws._

object TheseSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("TheseSpec")(
    suite("laws")(
      testM("associative")(checkAllLaws(Associative)(Gens.these(Gen.chunkOf(Gen.anyInt), Gen.chunkOf(Gen.anyInt)))),
      testM("associativeEither")(checkAllLaws(AssociativeEither)(GenFs.these(Gen.chunkOf(Gen.anyInt)), Gen.anyInt)),
      testM("commutative")(checkAllLaws(Commutative)(Gens.these(Gen.setOf(Gen.anyInt), Gen.setOf(Gen.anyInt)))),
      testM("commutativeBoth")(checkAllLaws(CommutativeBoth)(GenFs.these(Gen.setOf(Gen.anyInt)), Gen.anyInt)),
      testM("covariant")(checkAllLaws(Covariant)(GenFs.these(Gen.anyInt), Gen.anyInt)),
      testM("equal")(checkAllLaws(Equal)(Gens.these(Gen.anyInt, Gen.anyInt))),
      testM("hash")(checkAllLaws(Hash)(Gens.these(Gen.anyInt, Gen.anyInt))),
      testM("identityBoth")(checkAllLaws(IdentityBoth)(GenFs.these(Gen.chunkOf(Gen.anyInt)), Gen.anyInt)),
      testM("identityFlatten")(checkAllLaws(IdentityFlatten)(GenFs.these(Gen.chunkOf(Gen.anyInt)), Gen.anyInt)),
      testM("traversable")(checkAllLaws(Traversable)(GenFs.these(Gen.anyInt), Gen.anyInt))
    )
  )
}
