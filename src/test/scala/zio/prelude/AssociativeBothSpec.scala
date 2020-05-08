package zio.prelude

import zio.test._
import zio.test.laws._

object AssociativeBothSpec extends DefaultRunnableSpec {

  def spec = suite("AssociativeBothSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(AssociativeBoth)(GenFs.option, Gen.anyInt))
    )
  )
}
