package zio.prelude

import zio.test._
import zio.test.laws._

object AssociativeBothSpec extends DefaultRunnableSpec {

  type IntEither[+A] = Either[Int, A]

  implicitly[AssociativeBoth[IntEither]]

  def spec = suite("AssociativeBothSpec")(
    suite("laws")(
      testM("chunk")(checkAllLaws(AssociativeBoth)(GenF.chunk, Gen.chunkOf(Gen.anyString))),
      testM("either")(checkAllLaws(AssociativeBoth)(GenF.either(Gen.anyInt), Gen.anyInt)),
      testM("option")(checkAllLaws(AssociativeBoth)(GenF.option, Gen.anyInt)),
      testM("list")(checkAllLaws(AssociativeBoth)(GenF.list, Gen.anyString)),
      testM("vector")(checkAllLaws(AssociativeBoth)(GenF.vector, Gen.vectorOf(Gen.anyString)))
    )
  )
}
