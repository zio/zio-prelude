package zio.prelude

import zio.test._
import zio.test.laws._

object CovariantSpec extends DefaultRunnableSpec {

  def spec = suite("CovariantSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(Covariant)(GenFs.option, Gen.anyInt))
    )
  )
}
