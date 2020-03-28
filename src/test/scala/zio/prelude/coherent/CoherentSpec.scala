package zio.prelude.coherent

import zio.prelude._
import zio.test._
import zio.test.Assertion.{ isEmptyString, isTrue }

object CoherentSpec extends DefaultRunnableSpec {

  def spec = suite("CoherentSpec")(
    test("HashOrd") {
      val instance = implicitly[Hash[Int] with Ord[Int]]
      assert(instance.hash(0))(equalTo(0)) &&
      assert(instance.compare(0, 1))(equalTo(Ordering.LessThan))
    },
    test("IdentityEqual") {
      val instance = implicitly[Identity[String] with Equal[String]]

      assert(instance.identity)(isEmptyString) &&
      assert(instance.combine("a", "b"))(equalTo("ab")) &&
      assert(instance.equal("a", "a"))(isTrue)
    }
  )
}
