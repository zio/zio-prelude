package zio.prelude

import zio.prelude.Debug.Repr
import zio.test._

object DebugSpec extends DefaultRunnableSpec {

  def primitiveTest[A: Debug](a: A, exp: Option[String] = None) =
    assert(a.debug.render())(equalTo(exp.getOrElse(a.toString)))

  case class TestCase(string: String, number: Int, list: List[Double])
  val genTestCase = for {
    str    <- Gen.anyString
    number <- Gen.anyInt
    list   <- Gen.listOf(Gen.anyDouble)
  } yield TestCase(str, number, list)

  implicit val DebugTestCase = Debug.make[TestCase](a =>
    Repr.Constructor(
      List("DebugSpec"),
      "TestCase",
      Map("string" -> a.string.debug, "number" -> a.number.debug, "list" -> a.list.debug)
    )
  )

  sealed trait TestTrait
  case object TestObject1 extends TestTrait
  case object TestObject2 extends TestTrait

  implicit val testObject: Debug[TestTrait] = _ match {
    case TestObject1 => Repr.Object(List("DebugSpec"), "TestObject1")
    case TestObject2 => Repr.Object(List("DebugSpec"), "TestObject2")
  }

  val genTestTrait = Gen.elements(List[TestTrait](TestObject1, TestObject2): _*)

  def spec = suite("DebugSpec")(
    testM("unit")(check(Gen.unit)(primitiveTest(_, Some("scala.()")))),
    testM("int")(check(Gen.anyInt)(primitiveTest(_))),
    testM("double")(check(Gen.anyDouble)(primitiveTest(_))),
    testM("float")(check(Gen.anyFloat)(f => primitiveTest(f, Some(s"${f.toString}f")))),
    testM("long")(check(Gen.anyLong)(l => primitiveTest(l, Some(s"${l.toString}L")))),
    testM("byte")(check(Gen.anyByte)(primitiveTest(_))),
    testM("char")(check(Gen.anyChar)(primitiveTest(_))),
    testM("string")(check(Gen.anyString)(s => primitiveTest(s, Some(s""""$s"""")))),
    testM("list")(check(Gen.listOf(Gen.anyInt))(c => assert(c.debug.render())(equalTo(s"scala.${c.toString}")))),
    testM("caseclass")(
      check(genTestCase)(c =>
        assert(c.debug.render()) {
          val str = s""""${c.string}""""
          equalTo(s"DebugSpec.TestCase(string -> $str, number -> ${c.number}, list -> scala.${c.list})")
        }
      )
    ),
    testM("testTrait")(check(genTestTrait) { c =>
      val expected = c match {
        case TestObject1 => 1
        case TestObject2 => 2
      }
      assert(c.debug.render())(equalTo(s"DebugSpec.TestObject$expected"))
    })
  )
}
