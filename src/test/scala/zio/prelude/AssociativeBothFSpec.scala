package zio.prelude

import zio.test._
import zio.test.laws._

object AssociativeBothFSpec extends DefaultRunnableSpec {

  def spec = suite("AssociativeBothFSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(AssociativeBothF)(GenFs.option, Gen.anyInt))
    )
  )
}
