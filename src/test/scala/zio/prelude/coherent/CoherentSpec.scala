package zio.prelude.coherent

import zio.prelude._
import zio.prelude.newtypes.Sum
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
    },
    test("ClosureEqual") {
      val instance = implicitly[Closure[String] with Equal[String]]

      assert(instance.combine("a", "b"))(equalTo("ab")) &&
      assert(instance.equal("a", "a"))(isTrue)
    },
    test("AssociativeEqual") {
      val instance = implicitly[Associative[String] with Equal[String]]

      assert(instance.combine("a", "b"))(equalTo("ab")) &&
      assert(instance.equal("a", "a"))(isTrue)
    },
    test("CommutativeEqual") {
      val instance = implicitly[Commutative[Sum[Int]] with Equal[Sum[Int]]]

      assert(instance.combine(Sum(1), Sum(5)))(equalTo(Sum(6))) &&
      assert(instance.equal(Sum(5), Sum(5)))(isTrue)
    },
    test("AssociativeCommutativeEqual") {
      val instance = implicitly[Associative[Sum[Int]] with Commutative[Sum[Int]] with Equal[Sum[Int]]]

      assert(instance.combine(Sum(1), Sum(5)))(equalTo(Sum(6))) &&
      assert(instance.equal(Sum(5), Sum(5)))(isTrue)
    }
  )
}
