package zio.prelude

import zio.test._
import zio.test.laws._

object AssociativeEitherFSpec extends DefaultRunnableSpec {

  def spec = suite("AssociativeEitherFSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(AssociativeEitherF)(GenFs.option, Gen.anyInt))
    )
  )
}
