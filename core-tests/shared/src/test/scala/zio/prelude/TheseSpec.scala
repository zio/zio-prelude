package zio.prelude

import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object TheseSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] = suite("TheseSpec")(
    suite("laws")(
      test("associative")(checkAllLaws(AssociativeLaws)(Gens.these(Gen.chunkOf(Gen.int), Gen.chunkOf(Gen.int)))),
      test("covariant")(checkAllLaws(CovariantLaws)(GenFs.these(Gen.int), Gen.int)),
      test("equal")(checkAllLaws(EqualLaws)(Gens.these(Gen.int, Gen.int))),
      test("forEach")(checkAllLaws(ForEachLaws)(GenFs.these(Gen.int), Gen.int)),
      test("hash")(checkAllLaws(HashLaws)(Gens.these(Gen.int, Gen.int))),
      test("identityBoth")(checkAllLaws(IdentityBothLaws)(GenFs.these(Gen.chunkOf(Gen.int)), Gen.int)),
      test("identityFlatten")(checkAllLaws(IdentityFlattenLaws)(GenFs.these(Gen.chunkOf(Gen.int)), Gen.int))
    )
  )
}
