package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object TheseSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("TheseSpec")(
    suite("laws")(
      testM("associative")(checkAllLaws(AssociativeLaws)(Gens.these(Gen.chunkOf(Gen.anyInt), Gen.chunkOf(Gen.anyInt)))),
      testM("covariant")(checkAllLaws(CovariantLaws)(GenFs.these(Gen.anyInt), Gen.anyInt)),
      testM("equal")(checkAllLaws(EqualLaws)(Gens.these(Gen.anyInt, Gen.anyInt))),
      testM("forEach")(checkAllLaws(ForEachLaws)(GenFs.these(Gen.anyInt), Gen.anyInt)),
      testM("hash")(checkAllLaws(HashLaws)(Gens.these(Gen.anyInt, Gen.anyInt))),
      testM("identityBoth")(checkAllLaws(IdentityBothLaws)(GenFs.these(Gen.chunkOf(Gen.anyInt)), Gen.anyInt)),
      testM("identityFlatten")(checkAllLaws(IdentityFlattenLaws)(GenFs.these(Gen.chunkOf(Gen.anyInt)), Gen.anyInt))
    )
  )
}
