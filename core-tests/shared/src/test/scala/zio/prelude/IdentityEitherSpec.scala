package zio.prelude

import zio.Trace
import zio.prelude.laws.IdentityEitherlaws
import zio.prelude.data.Optional
import zio.test._
import zio.test.laws._

object IdentityEitherSpec extends ZIOBaseSpec {

  val optional: GenF[Any, Optional] =
    new GenF[Any, Optional] {
      def apply[R1, A](gen: Gen[R1, A])(implicit trace: Trace): Gen[R1, Optional[A]] =
        Gen.option(gen)
    }

  def spec: Spec[Environment, Any] =
    suite("IdentityEitherSpec")(
      suite("laws")(
        test("chunk")(checkAllLaws(IdentityEitherlaws)(GenF.chunk, Gen.int)),
        test("list")(checkAllLaws(IdentityEitherlaws)(GenF.list, Gen.int)),
        test("option")(checkAllLaws(IdentityEitherlaws)(GenF.option, Gen.int)),
        test("optional")(checkAllLaws(IdentityEitherlaws)(optional, Gen.int)),
        test("set")(checkAllLaws(IdentityEitherlaws)(GenF.set, Gen.int)),
        test("vector")(checkAllLaws(IdentityEitherlaws)(GenF.vector, Gen.int))
      )
    )
}
