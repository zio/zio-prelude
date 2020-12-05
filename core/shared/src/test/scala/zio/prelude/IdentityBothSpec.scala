package zio.prelude

import zio.test.laws._
import zio.test.{ testM, _ }

object IdentityBothSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityBothSpec")(
      suite("laws")(
        testM("either")(checkAllLaws(IdentityBoth)(GenF.either(Gen.anyInt), Gen.anyInt)),
        testM("list")(checkAllLaws(IdentityBoth)(GenF.list, Gen.anyInt)),
        testM("option")(checkAllLaws(IdentityBoth)(GenF.option, Gen.anyInt)),
        testM("try")(checkAllLaws(IdentityBoth)(GenFs.tryScala, Gen.anyInt)), {
          implicit val invariant = Covariant.NestedCovariant[List, Option]
          testM("Nested[list,option]")(checkAllLaws(IdentityBoth)(GenFs.nested(GenF.list, GenF.option), Gen.anyInt))
        }
      )
    )
}
