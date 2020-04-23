package zio.prelude

import zio.test._
import zio.test.laws._

object CommutativeBothFSpec extends DefaultRunnableSpec {

  def spec = suite("CommutativeBothFSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(CommutativeBothF)(GenFs.option, Gen.anyInt))
    )
  )
}
