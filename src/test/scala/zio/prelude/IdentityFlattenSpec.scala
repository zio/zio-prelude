package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.laws._

object IdentityFlattenSpec extends DefaultRunnableSpec {

  IdentityFlatten[Vector]

  val genFEither: GenF[Random with Sized, ({ type lambda[+x] = Either[Int, x] })#lambda] =
    GenF.either(Gen.anyInt)

  def spec = suite("IdentityFlattenSpec")(
    suite("laws")(
      testM("chunk")(checkAllLaws(IdentityFlatten)(GenF.chunk, Gen.chunkOf(Gen.anyString))),
      testM("either")(checkAllLaws(IdentityFlatten)(genFEither, Gen.anyInt)),
      testM("option")(checkAllLaws(IdentityFlatten)(GenF.option, Gen.anyInt)),
      testM("list")(checkAllLaws(IdentityFlatten)(GenF.list, Gen.anyString)),
      testM("vector")(checkAllLaws(IdentityFlatten)(GenF.vector, Gen.anyString))
    )
  )

}
