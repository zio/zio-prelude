package zio.prelude

import zio.prelude.IdentityFlattenSpec.genFEither
import zio.prelude.CovariantSpec.genFMap
import zio.test._
import zio.test.laws._

object AssociativeFlattenSpec extends DefaultRunnableSpec {

  def spec = suite("AssociativeFlattenSpec")(
    suite("laws")(
      testM("chunk")(checkAllLaws(AssociativeFlatten)(GenF.chunk, Gen.chunkOf(Gen.anyString))),
      testM("either")(checkAllLaws(AssociativeFlatten)(genFEither, Gen.anyInt)),
      testM("option")(checkAllLaws(AssociativeFlatten)(GenF.option, Gen.anyInt)),
      testM("list")(checkAllLaws(AssociativeFlatten)(GenF.list, Gen.anyString)),
      testM("vector")(checkAllLaws(AssociativeFlatten)(GenF.vector, Gen.anyString)),
      testM("map")(checkAllLaws(AssociativeFlatten)(genFMap, Gen.anyInt))
    )
  )
}
