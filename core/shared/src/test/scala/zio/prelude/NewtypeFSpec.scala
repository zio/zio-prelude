package zio.prelude

import zio.test.Assertion.anything
import zio.test.AssertionM.Render.param
import zio.test.{Assertion, DefaultRunnableSpec, ZSpec, assert, suite, test}

object NewtypeFSpec extends DefaultRunnableSpec {

  def isShorterThan(length: Int): AssertionF[List] = new AssertionF[List] {
    def apply[x]: Assertion[List[x]] = Assertion.assertion("isShorterThan")(param(length))(_.length < length)
  }

  def spec: ZSpec[Environment, Failure] =
    suite("NewtypeFSpec")(
      suite("NewtypeSmartF")(
        test("valid values") {
          assert(ShortList.make(List(1, 2, 3, 4)))(isSuccessV(anything))
        },
        test("invalid values") {
          val expected = NonEmptyMultiSet("List(1, 2, 3, 4, 5) did not satisfy isShorterThan(5)")
          assert(ShortList.make(List(1, 2, 3, 4, 5)))(isFailureV(equalTo(expected)))
        }
      ),
      suite("SubtypeSmartF")(
        test("subtypes values") {
          assert((ShortList.list1234: List[Int]) ++ List(5, 6))(equalTo(List(1, 2, 3, 4) ++ List(5, 6)))
        }
      )
    )

  object ShortList extends SubtypeSmartF[List](isShorterThan(5)) {
    val list1234: ShortList[Int] = ShortList[Int](List(1, 2, 3, 4))
  }
  type ShortList[x] = ShortList.Type[x]

}
