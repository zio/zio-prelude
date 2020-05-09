package zio.prelude

import zio.test._
import zio.test.laws._

object CommutativeBothSpec extends DefaultRunnableSpec {

  def spec = suite("CommutativeBothSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(CommutativeBoth)(GenFs.option, Gen.anyInt))
    )
  )
}
