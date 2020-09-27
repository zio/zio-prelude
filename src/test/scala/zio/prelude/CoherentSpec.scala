package zio.prelude

import zio.prelude.coherent._
import zio.prelude.newtypes.Sum
import zio.test.Assertion.{ isEmptyString, isTrue }
import zio.test._

object CoherentSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("CoherentSpec")(
      test("HashOrd") {
        val instance = implicitly[HashOrd[Double]].contramap[Int](_.toDouble)
        assert(instance.hash(0))(isEqualTo(0)) &&
        assert(instance.compare(0, 1))(isEqualTo(Ordering.LessThan))
      },
      test("IdentityEqual") {
        val instance = implicitly[EqualIdentity[String]]

        assert(instance.identity)(isEmptyString) &&
        assert(instance.combine("a", "b"))(isEqualTo("ab")) &&
        assert(instance.equal("a", "a"))(isTrue)
      },
      test("AssociativeEqual") {
        val instance = implicitly[AssociativeEqual[String]]

        assert(instance.combine("a", "b"))(isEqualTo("ab")) &&
        assert(instance.equal("a", "a"))(isTrue)
      },
      test("CommutativeEqual") {
        val instance = implicitly[CommutativeEqual[Sum[Int]]]

        assert(instance.combine(Sum(1), Sum(5)))(isEqualTo(Sum(6))) &&
        assert(instance.equal(Sum(5), Sum(5)))(isTrue)
      }
    )
}
