package zio.prelude

import zio.test._
import zio.test.laws._

object AssociativeFSpec extends DefaultRunnableSpec {

  def spec = suite("CovariantSpec")(
    suite("laws")(
      suite("both")(
        testM("option")(checkAllLaws(AssociativeF.Both)(GenFs.option, Gen.anyInt))
      ),
      suite("either")(
        testM("option")(checkAllLaws(AssociativeF.Either)(GenFs.option, Gen.anyInt))
      )
    )
  )
}
