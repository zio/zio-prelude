package zio.prelude

import zio.test._
import zio.test.laws._

object AssociativeFlattenSpec extends DefaultRunnableSpec {

  def spec = suite("AssociativeFlattenSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(AssociativeFlatten)(GenF.option, Gen.anyInt))
    )
  )
}
